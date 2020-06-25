namespace GWallet.Backend.UtxoCoin.Lightning

open System
open System.Collections
open System.Net
open NBitcoin

open DotNetLightning.Serialize.Msgs
open DotNetLightning.Serialize
open DotNetLightning.Utils

open GWallet.Backend
open GWallet.Backend.FSharpUtil
open GWallet.Backend.FSharpUtil.UwpHacks
open GWallet.Backend.UtxoCoin.Lightning.Util
open GWallet.Backend.UtxoCoin.Lightning.Primitives
open FSharp.Core

type InitializeError =
    | ReceiveInit of RecvBytesError
    | DeserializeInit of P2PDecodeError
    | UnexpectedMsg of ILightningMsg
    interface IErrorMsg with
        member this.Message =
            match this with
            | ReceiveInit err ->
                SPrintF1 "Error receiving init message: %s" (err :> IErrorMsg).Message
            | DeserializeInit err ->
                SPrintF1 "Error deserializing init message: %s" err.Message
            | UnexpectedMsg msg ->
                SPrintF1 "Expected init message, got %A" (msg.GetType())
    member internal this.PossibleBug =
        match this with
        | ReceiveInit err -> err.PossibleBug
        | DeserializeInit _
        | UnexpectedMsg _ -> false

type ConnectError =
    | Handshake of HandshakeError
    | Initialize of InitializeError
    interface IErrorMsg with
        member this.Message =
            match this with
            | Handshake err ->
                SPrintF1 "Handshake failed: %s" (err :> IErrorMsg).Message
            | Initialize err ->
                SPrintF1 "Message stream initialization failed: %s" (err :> IErrorMsg).Message
    member internal this.PossibleBug =
        match this with
        | Handshake err -> err.PossibleBug
        | Initialize err -> err.PossibleBug

type RecvMsgError =
    | RecvBytes of RecvBytesError
    | DeserializeMsg of P2PDecodeError
    interface IErrorMsg with
        member this.Message =
            match this with
            | RecvBytes err ->
                SPrintF1 "Error receiving raw data from peer: %s" (err :> IErrorMsg).Message
            | DeserializeMsg err ->
                SPrintF1 "Error deserializing message from peer: %s" err.Message
    member internal this.PossibleBug =
        match this with
        | RecvBytes err -> err.PossibleBug
        | DeserializeMsg _ -> false

type internal MsgStream = {
    TransportStream: TransportStream
} with
    interface IDisposable with
        member this.Dispose() =
            (this.TransportStream :> IDisposable).Dispose()

    static member internal SupportedFeatures: FeatureBit =
        let featureBits = FeatureBit.Zero
        featureBits.SetFeature Feature.OptionDataLossProtect FeaturesSupport.Optional true
        featureBits

    static member private InitializeTransportStream (transportStream: TransportStream)
                                                        : Async<Result<Init * MsgStream, InitializeError>> = async {
        let! transportStreamAfterInitSent =
            let plainInit: Init = {
                Features = MsgStream.SupportedFeatures
                TLVStream = [||]
            }
            let msg = plainInit :> ILightningMsg
            let bytes = msg.ToBytes()
            transportStream.SendBytes bytes

        let! transportStreamAfterInitReceivedRes = transportStreamAfterInitSent.RecvBytes()
        match transportStreamAfterInitReceivedRes with
        | Error recvBytesError -> return Error <| ReceiveInit recvBytesError
        | Ok (transportStreamAfterInitReceived, bytes) ->
            match LightningMsg.fromBytes bytes with
            | Error msgError -> return Error <| DeserializeInit msgError
            | Ok msg ->
                match msg with
                | :? Init as init ->
                    let msgStream = { TransportStream = transportStreamAfterInitReceived }
                    Infrastructure.LogDebug <| SPrintF1 "peer init features == %s" (init.Features.PrettyPrint)
                    return Ok (init, msgStream)
                | _ -> return Error <| UnexpectedMsg msg
    }

    static member internal Connect (nodeSecret: ExtKey)
                          (peerNodeId: NodeId)
                          (peerId: PeerId)
                              : Async<Result<Init * MsgStream, ConnectError>> = async {
        let! transportStreamRes =
            TransportStream.Connect
                nodeSecret
                peerNodeId
                peerId
        match transportStreamRes with
        | Error handshakeError -> return Error <| Handshake handshakeError
        | Ok transportStream -> 
            let! initializeRes = MsgStream.InitializeTransportStream transportStream
            match initializeRes with
            | Error initializeError -> return Error <| Initialize initializeError
            | Ok (init, msgStream) -> return Ok (init, msgStream)
    }

    static member internal AcceptFromTransportListener (transportListener: TransportListener)
                                                  : Async<Result<Init * MsgStream, ConnectError>> = async {
        let! transportStreamRes =
            TransportStream.AcceptFromTransportListener transportListener
        match transportStreamRes with
        | Error handshakeError -> return Error <| Handshake handshakeError
        | Ok transportStream ->
            let! initializeRes = MsgStream.InitializeTransportStream transportStream
            match initializeRes with
            | Error initializeError -> return Error <| Initialize initializeError
            | Ok (init, msgStream) -> return Ok (init, msgStream)
    }

    member this.RemoteNodeId
        with get(): NodeId = this.TransportStream.RemoteNodeId

    member internal this.PeerId
        with get(): PeerId = this.TransportStream.PeerId

    member this.RemoteEndPoint
        with get(): IPEndPoint = this.TransportStream.RemoteEndPoint

    member this.LnEndPoint: LnEndPoint =
        this.TransportStream.LnEndPoint

    member internal this.NodeSecret =
        this.TransportStream.NodeSecret

    member internal this.SendMsg (msg: ILightningMsg): Async<MsgStream> = async {
        let bytes = msg.ToBytes()
        let! transportStream = this.TransportStream.SendBytes bytes
        return { this with TransportStream = transportStream }
    }

    member internal this.RecvMsg(): Async<Result<MsgStream * ILightningMsg, RecvMsgError>> = async {
        let! recvBytesRes = this.TransportStream.RecvBytes()
        match recvBytesRes with
        | Error recvBytesError -> return Error <| RecvBytes recvBytesError
        | Ok (transportStream, bytes) ->
            match LightningMsg.fromBytes bytes with
            | Error msgError -> return Error <| DeserializeMsg msgError
            | Ok msg ->
                return Ok ({ this with TransportStream = transportStream }, msg)
    }

