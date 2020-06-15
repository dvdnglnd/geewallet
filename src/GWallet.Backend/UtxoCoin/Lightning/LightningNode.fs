namespace GWallet.Backend.UtxoCoin.Lightning

open System.IO
open System.Net

open NBitcoin

open GWallet.Backend
open GWallet.Backend.UtxoCoin

type NodeOpenChannelError =
    | Connect of ConnectError
    | OpenChannel of OpenChannelError

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

    member this.OpenChannel (peerNodeId: NodeId)
                            (peerEndPoint: IPEndPoint)
                            (channelCapacity: TransferAmount)
                            (metadata: TransactionMetadata)
                            (password: string)
                                : Async<Result<OutgoingUnfundedChannel, NodeOpenChannelError>> = async {
        let peerId = PeerId (peerEndPoint :> EndPoint)
        let! connectRes =
            PeerWrapper.Connect this.SecretKey peerNodeId peerId
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



