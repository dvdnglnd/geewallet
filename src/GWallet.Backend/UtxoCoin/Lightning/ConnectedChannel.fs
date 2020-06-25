namespace GWallet.Backend.UtxoCoin.Lightning

open System
open System.Net
open System.IO

open NBitcoin

open DotNetLightning.Serialize.Msgs
open DotNetLightning.Channel
open DotNetLightning.Utils

open GWallet.Backend
open GWallet.Backend.FSharpUtil
open GWallet.Backend.FSharpUtil.UwpHacks
open GWallet.Backend.UtxoCoin
open GWallet.Backend.UtxoCoin.Lightning.Util
open GWallet.Backend.UtxoCoin.Lightning.Primitives

open FSharp.Core

type internal ReestablishError =
    | RecvReestablish of RecvMsgError
    | PeerErrorResponse of PeerWrapper * PeerErrorMessage
    | ExpectedReestablishMsg of ILightningMsg
    | ExpectedReestablishOrFundingLockedMsg of ILightningMsg
    interface IErrorMsg with
        member this.Message =
            match this with
            | RecvReestablish err ->
                SPrintF1 "Error receiving channel_reestablish: %s" (err :> IErrorMsg).Message
            | PeerErrorResponse (_, err) ->
                SPrintF1 "Peer responded to our channel_reestablish with an error: %s" (err :> IErrorMsg).Message
            | ExpectedReestablishMsg msg ->
                SPrintF1 "Expected channel_reestablish, got %A" (msg.GetType())
            | ExpectedReestablishOrFundingLockedMsg msg ->
                SPrintF1 "Expected channel_reestablish or funding_locked, got %A" (msg.GetType())
    member internal this.PossibleBug =
        match this with
        | RecvReestablish err -> err.PossibleBug
        | PeerErrorResponse _
        | ExpectedReestablishMsg _
        | ExpectedReestablishOrFundingLockedMsg _ -> false

type internal ReconnectError =
    | Connect of ConnectError
    | Reestablish of ReestablishError
    interface IErrorMsg with
        member this.Message =
            match this with
            | Connect err ->
                SPrintF1 "Error reconnecting to peer: %s" (err :> IErrorMsg).Message
            | Reestablish err ->
                SPrintF1 "Error reestablishing channel with connected peer: %s" (err :> IErrorMsg).Message
    member internal this.PossibleBug =
        match this with
        | Connect err -> err.PossibleBug
        | Reestablish err -> err.PossibleBug

type internal ConnectedChannel = {
    PeerWrapper: PeerWrapper
    ChannelWrapper: ChannelWrapper
    Account: NormalUtxoAccount
    MinimumDepth: BlockHeightOffset32
    ChannelIndex: int
} with
    interface IDisposable with
        member this.Dispose() =
            (this.PeerWrapper :> IDisposable).Dispose()

    static member private LoadChannel (channelStore: ChannelStore)
                                      (nodeSecretKey: ExtKey)
                                      (channelId: ChannelIdWrapper)
                                          : Async<SerializedChannel * ChannelWrapper> = async {
        let serializedChannel = channelStore.LoadChannel channelId
        Infrastructure.LogDebug <| SPrintF1 "loading channel for %s" (channelId.ToString())
        let! channelWrapper =
            let fundingTxProvider (_ : IDestination * Money * FeeRatePerKw) =
                Result.Error "funding tx not needed cause channel already created"
            ChannelWrapper.Create
                serializedChannel.RemoteNodeId
                (Account.CreatePayoutScript channelStore.Account)
                nodeSecretKey
                serializedChannel.ChannelIndex
                fundingTxProvider
                serializedChannel.ChanState
        return serializedChannel, channelWrapper
    }

    static member private Reestablish (peerWrapper: PeerWrapper)
                                      (channelWrapper: ChannelWrapper)
                                          : Async<Result<PeerWrapper * ChannelWrapper, ReestablishError>> = async {
        let channelId =
            match channelWrapper.ChannelId with
            | Some channelId -> channelId
            | None ->
                failwith
                    "A channel can only be reestablished if it has previously been \
                    established and therefore has a channel id"

        let ourReestablishMsgRes, channelWrapperAfterReestablishSent =
            let channelCmd = ChannelCommand.CreateChannelReestablish
            channelWrapper.ExecuteCommand channelCmd <| function
                | (WeSentChannelReestablish(ourReestablishMsg)::[]) ->
                    Some ourReestablishMsg
                | _ -> None
        let ourReestablishMsg = Unwrap ourReestablishMsgRes "error executing channel reestablish command"

        Infrastructure.LogDebug <| SPrintF1 "sending reestablish for %s" (channelId.ToString())
        let! peerWrapperAfterReestablishSent = peerWrapper.SendMsg ourReestablishMsg
        Infrastructure.LogDebug <| SPrintF1 "receiving reestablish for %s" (channelId.ToString())
        let! reestablishRes = async {
            let! recvMsgRes = peerWrapperAfterReestablishSent.RecvChannelMsg()
            match recvMsgRes with
            | Error (RecvMsg recvMsgError) -> return Error <| RecvReestablish recvMsgError
            | Error (ReceivedPeerErrorMessage (peerWrapperAfterNextMsgReceived, errorMessage)) ->
                return Error <| PeerErrorResponse (peerWrapperAfterNextMsgReceived, errorMessage)
            | Ok (peerWrapperAfterNextMsgReceived, channelMsg) ->
                match channelMsg with
                | :? ChannelReestablish as reestablishMsg ->
                    return Ok (peerWrapperAfterNextMsgReceived, reestablishMsg)
                | :? FundingLocked ->
                    let! recvMsgRes = peerWrapperAfterNextMsgReceived.RecvChannelMsg()
                    match recvMsgRes with
                    | Error (RecvMsg recvMsgError) -> return Error <| RecvReestablish recvMsgError
                    | Error (ReceivedPeerErrorMessage (peerWrapperAfterReestablishReceived, errorMessage)) ->
                        return Error <| PeerErrorResponse
                            (peerWrapperAfterReestablishReceived, errorMessage)
                    | Ok (peerWrapperAfterReestablishReceived, channelMsg) ->
                        match channelMsg with
                        | :? ChannelReestablish as reestablishMsg ->
                            return Ok (peerWrapperAfterReestablishReceived, reestablishMsg)
                        | msg ->
                            return Error <| ExpectedReestablishMsg msg
                | msg ->
                    return Error <| ExpectedReestablishOrFundingLockedMsg msg
        }
        match reestablishRes with
        | Error err -> return Error err
        | Ok (peerWrapperAfterReestablishReceived, _theirReestablishMsg) ->
            // TODO: check their reestablish msg
            //
            // A channel_reestablish message contains the channel ID as well as
            // information specifying what state the remote node thinks the channel
            // is in. So we need to check that the channel IDs match, validate that
            // the information they've sent us makes sense, and possibly re-send
            // commitments. Aside from checking the channel ID this is the sort of
            // thing that should be handled by DNL, except DNL doesn't have an
            // ApplyChannelReestablish command.
            return Ok (peerWrapperAfterReestablishReceived, channelWrapperAfterReestablishSent)
    }

    static member internal ConnectFromWallet (channelStore: ChannelStore)
                                    (nodeSecretKey: ExtKey)
                                    (channelId: ChannelIdWrapper)
                                        : Async<Result<ConnectedChannel, ReconnectError>> = async {
        let! serializedChannel, channelWrapper =
            ConnectedChannel.LoadChannel channelStore nodeSecretKey channelId
        let! connectRes =
            let nodeId = channelWrapper.RemoteNodeId
            let peerId = PeerId (serializedChannel.CounterpartyIP :> EndPoint)
            PeerWrapper.Connect
                nodeSecretKey
                nodeId
                peerId
        match connectRes with
        | Error connectError -> return Error <| Connect connectError
        | Ok peerWrapper ->
            let! reestablishRes =
                ConnectedChannel.Reestablish peerWrapper channelWrapper
            match reestablishRes with
            | Error reestablishError -> return Error <| Reestablish reestablishError
            | Ok (peerWrapperAfterReestablish, channelWrapperAfterReestablish) ->
                let minimumDepth = serializedChannel.MinSafeDepth
                let channelIndex = serializedChannel.ChannelIndex
                let connectedChannel = {
                    Account = channelStore.Account
                    ChannelWrapper = channelWrapperAfterReestablish
                    PeerWrapper = peerWrapperAfterReestablish
                    MinimumDepth = minimumDepth
                    ChannelIndex = channelIndex
                }
                return Ok connectedChannel
    }

    static member internal AcceptFromWallet (channelStore: ChannelStore)
                                   (transportListener: TransportListener)
                                   (channelId: ChannelIdWrapper)
                                       : Async<Result<ConnectedChannel, ReconnectError>> = async {
        let! serializedChannel, channelWrapper =
            ConnectedChannel.LoadChannel channelStore transportListener.NodeSecret channelId
        let! connectRes =
            let nodeId = channelWrapper.RemoteNodeId
            PeerWrapper.AcceptFromTransportListener
                transportListener
                nodeId
        match connectRes with
        | Error connectError -> return Error <| Connect connectError
        | Ok peerWrapper ->
            let! reestablishRes =
                ConnectedChannel.Reestablish peerWrapper channelWrapper
            match reestablishRes with
            | Error reestablishError -> return Error <| Reestablish reestablishError
            | Ok (peerWrapperAfterReestablish, channelWrapperAfterReestablish) ->
                let minimumDepth = serializedChannel.MinSafeDepth
                let channelIndex = serializedChannel.ChannelIndex
                let connectedChannel = {
                    Account = channelStore.Account
                    ChannelWrapper = channelWrapperAfterReestablish
                    PeerWrapper = peerWrapperAfterReestablish
                    MinimumDepth = minimumDepth
                    ChannelIndex = channelIndex
                }
                return Ok connectedChannel
    }

    member this.SaveToWallet() =
        let channelStore = ChannelStore this.Account
        let serializedChannel = {
            ChannelIndex = this.ChannelIndex
            Network = this.ChannelWrapper.Network
            RemoteNodeId = this.PeerWrapper.RemoteNodeId
            ChanState = this.ChannelWrapper.Channel.State
            AccountFileName = this.Account.AccountFile.Name
            CounterpartyIP = this.PeerWrapper.RemoteEndPoint
            MinSafeDepth = this.MinimumDepth
        }
        channelStore.SaveChannel serializedChannel

    member this.RemoteNodeId
        with get(): NodeIdWrapper = this.ChannelWrapper.RemoteNodeId

    member internal this.Network
        with get(): Network = this.ChannelWrapper.Network

    member this.ChannelId
        with get(): ChannelIdWrapper =
            UnwrapOption
                this.ChannelWrapper.ChannelId
                "A ConnectedChannel guarantees that a channel is connected and \
                therefore has a channel id"

    member this.FundingTxId
        with get(): TxIdWrapper =
            UnwrapOption
                this.ChannelWrapper.FundingTxId
                "A ConnectedChannel guarantees that a channel has been \
                established and therefore has a funding txid"

    member internal this.FundingScriptCoin
        with get(): Option<ScriptCoin> = this.ChannelWrapper.FundingScriptCoin

    member this.SendError (err: string): Async<ConnectedChannel> = async {
        let errorMsg = {
            ChannelId =
                match this.ChannelWrapper.Channel.State.ChannelId with
                | Some channelId -> WhichChannel.SpecificChannel channelId
                | _ -> WhichChannel.All
            Data = System.Text.Encoding.ASCII.GetBytes err
        }
        let! peerWrapper = this.PeerWrapper.SendMsg errorMsg
        return {
            this with
                PeerWrapper = peerWrapper
        }
    }

