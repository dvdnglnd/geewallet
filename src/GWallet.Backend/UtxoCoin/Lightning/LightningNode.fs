namespace GWallet.Backend.UtxoCoin.Lightning

open System.IO
open System.Net

open NBitcoin

open GWallet.Backend
open GWallet.Backend.UtxoCoin
open GWallet.Backend.FSharpUtil.UwpHacks

type NodeOpenChannelError =
    | Connect of ConnectError
    | OpenChannel of OpenChannelError
    with
    member this.Message =
        match this with
        | Connect connectError ->
            SPrintF1 "error connecting: %s" connectError.Message
        | OpenChannel openChannelError ->
            SPrintF1 "error opening channel: %s" openChannelError.Message
    member this.PossibleBug =
        match this with
        | Connect connectError -> connectError.PossibleBug
        | OpenChannel openChannelError -> openChannelError.PossibleBug

type LightningNode internal (channelStore: ChannelStore, transportListener: TransportListener) =
    static member Start (channelStore: ChannelStore)
                        (password: string)
                        (bindAddress: IPEndPoint)
                            : LightningNode =
        let secretKey: ExtKey = 
            let privateKey = Account.GetPrivateKey channelStore.Account password
            //let bytes: array<byte> = Array.zeroCreate Key.BytesLength
            let bytes: array<byte> = Array.zeroCreate 32
            use bytesStream = new MemoryStream(bytes)
            let stream = NBitcoin.BitcoinStream(bytesStream, true)
            privateKey.ReadWrite stream
            NBitcoin.ExtKey bytes
        let transportListener = TransportListener.Bind secretKey bindAddress
        LightningNode(channelStore, transportListener)

    member this.ChannelStore = channelStore
    member this.TransportListener = transportListener
    member this.SecretKey = this.TransportListener.NodeSecret
    member this.Account = this.ChannelStore.Account

    member this.OpenChannel (lnEndPoint: LnEndPoint)
                            (channelCapacity: TransferAmount)
                            (metadata: TransactionMetadata)
                            (password: string)
                                : Async<Result<OutgoingUnfundedChannel, NodeOpenChannelError>> = async {
        let peerId = PeerId (lnEndPoint.IPEndPoint :> EndPoint)
        let! connectRes =
            PeerWrapper.Connect this.SecretKey lnEndPoint.NodeId peerId
        match connectRes with
        | Error connectError ->
            return Error <| NodeOpenChannelError.Connect connectError
        | Ok peerWrapper ->
            let! outgoingUnfundedChannelRes =
                OutgoingUnfundedChannel.OpenChannel
                    peerWrapper
                    this.Account
                    channelCapacity
                    metadata
                    password
            match outgoingUnfundedChannelRes with
            | Error openChannelError ->
                return Error <| NodeOpenChannelError.OpenChannel openChannelError
            | Ok outgoingUnfundedChannel ->
                return Ok outgoingUnfundedChannel
    }

module LightningNode =
    let Start = LightningNode.Start
    let OpenChannel (lightningNode: LightningNode) = lightningNode.OpenChannel

