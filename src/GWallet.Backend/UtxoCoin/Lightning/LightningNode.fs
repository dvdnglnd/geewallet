namespace GWallet.Backend.UtxoCoin.Lightning

open System
open System.IO
open System.Net

open NBitcoin
open DotNetLightning.Utils

open GWallet.Backend
open GWallet.Backend.UtxoCoin
open GWallet.Backend.FSharpUtil.UwpHacks
open GWallet.Backend.UtxoCoin.Lightning.Primitives

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

type NodeAcceptChannelError =
    | AcceptPeer of ConnectError
    | AcceptChannel of AcceptChannelError
    with
    member this.Message =
        match this with
        | AcceptPeer connectError ->
            SPrintF1 "error accepting connection: %s" connectError.Message
        | AcceptChannel acceptChannelError ->
            SPrintF1 "error accepting channel: %s" acceptChannelError.Message

type NodeSendMonoHopPaymentError =
    | Reconnect of ReconnectActiveChannelError
    | SendPayment of SendMonoHopPaymentError
    member this.Message =
        match this with
        | Reconnect reconnectActiveChannelError ->
            SPrintF1 "error reconnecting channel: %s" reconnectActiveChannelError.Message
        | SendPayment sendMonoHopPaymentError ->
            SPrintF1 "error sending payment on reconnected channel: %s" sendMonoHopPaymentError.Message

type NodeReceiveMonoHopPaymentError =
    | Reconnect of ReconnectActiveChannelError
    | ReceivePayment of RecvMonoHopPaymentError
    member this.Message =
        match this with
        | Reconnect reconnectActiveChannelError -> 
            SPrintF1 "error reconnecting channel: %s" reconnectActiveChannelError
        | ReceivePayment recvMonoHopPaymentError ->
            SPrintF1 "error receiving payment on reconnected channel: %s" recvMonoHopPaymentError

type PendingChannel internal (outgoingUnfundedChannel: OutgoingUnfundedChannel) =
    member internal this.OutgoingUnfundedChannel = outgoingUnfundedChannel
    member this.MinimumDepth: uint32 =
        this.OutgoingUnfundedChannel.MinimumDepth.Value
    member this.Accept (): Async<Result<TxId, FundChannelError>> = async {
        let! fundedChannelRes =
            FundedChannel.FundChannel this.OutgoingUnfundedChannel
        match fundedChannelRes with
        | Error fundChannelError ->
            if fundChannelError.PossibleBug then
                let msg = SPrintF1 "Error funding channel: %s" fundChannelError.Message
                Infrastructure.ReportWarningMessage msg
            return Error fundChannelError
        | Ok fundedChannel ->
            let txId = fundedChannel.FundingTxId
            (fundedChannel :> IDisposable).Dispose()
            return Ok txId
    }

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
        new LightningNode(channelStore, transportListener)

    member this.ChannelStore = channelStore
    member this.TransportListener = transportListener
    member this.SecretKey = this.TransportListener.NodeSecret
    member this.NodeId = this.TransportListener.NodeId
    member this.LnEndPoint = this.TransportListener.LnEndPoint
    member this.Account = this.ChannelStore.Account

    interface IDisposable with
        member this.Dispose() =
            (this.TransportListener :> IDisposable).Dispose()

    member this.OpenChannel (lnEndPoint: LnEndPoint)
                            (channelCapacity: TransferAmount)
                            (metadata: TransactionMetadata)
                            (password: string)
                                : Async<Result<PendingChannel, NodeOpenChannelError>> = async {
        let peerId = PeerId (lnEndPoint.IPEndPoint :> EndPoint)
        let! connectRes =
            PeerWrapper.Connect this.SecretKey lnEndPoint.NodeId peerId
        match connectRes with
        | Error connectError ->
            if connectError.PossibleBug then
                let msg =
                    SPrintF3
                        "error connecting to peer %s to open channel of capacity %M: %s"
                        (lnEndPoint.ToString())
                        channelCapacity.ValueToSend
                        connectError.Message
                Infrastructure.ReportWarningMessage msg
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
                if openChannelError.PossibleBug then
                    let msg =
                        SPrintF3
                            "error opening channel with peer %s of capacity %M: %s"
                            (lnEndPoint.ToString())
                            channelCapacity.ValueToSend
                            openChannelError.Message
                    Infrastructure.ReportWarningMessage msg
                return Error <| NodeOpenChannelError.OpenChannel openChannelError
            | Ok outgoingUnfundedChannel ->
                return Ok <| PendingChannel outgoingUnfundedChannel
    }

    member this.AcceptChannel (): Async<Result<TxId, NodeAcceptChannelError>> = async {
        let! acceptPeerRes =
            PeerWrapper.AcceptAnyFromTransportListener this.TransportListener
        match acceptPeerRes with
        | Error connectError ->
            if connectError.PossibleBug then
                let msg =
                    SPrintF1
                        "error accepting connection from peer to accept channel: %s"
                        connectError.Message
                Infrastructure.ReportWarningMessage msg
            return Error <| NodeAcceptChannelError.AcceptPeer connectError
        | Ok peerWrapper ->
            let! fundedChannelRes = FundedChannel.AcceptChannel peerWrapper this.Account
            match fundedChannelRes with
            | Error acceptChannelError ->
                if acceptChannelError.PossibleBug then
                    let msg =
                        SPrintF2
                            "error accepting channel from peer %s: %s"
                            (peerWrapper.LnEndPoint.ToString())
                            acceptChannelError.Message
                    Infrastructure.ReportWarningMessage msg
                return Error <| NodeAcceptChannelError.AcceptChannel acceptChannelError
            | Ok fundedChannel ->
                let txId = fundedChannel.FundingTxId
                (fundedChannel :> IDisposable).Dispose()
                return Ok txId
    }

    member this.SendMonoHopPayment (channelId: ChannelId)
                                   (transferAmount: TransferAmount)
                                       : Async<Result<unit, NodeSendMonoHopPaymentError>> = async {
        let amount =
            let btcAmount = transferAmount.ValueToSend
            let lnAmount = int64(btcAmount * decimal DotNetLightning.Utils.LNMoneyUnit.BTC)
            DotNetLightning.Utils.LNMoney lnAmount
        let! activeChannelRes = ActiveChannel.ConnectReestablish this.ChannelStore this.SecretKey channelId
        match activeChannelRes with
        | Error reconnectActiveChannelError ->
            if reconnectActiveChannelError.PossibleBug then
                let msg =
                    SPrintF2
                        "error connecting to peer to send monohop payment on channel %s: %s"
                        (channelId.ToString())
                        reconnectActiveChannelError.Message
                Infrastructure.ReportWarningMessage msg
            return Error <| NodeSendMonoHopPaymentError.Reconnect reconnectActiveChannelError
        | Ok activeChannel ->
            let! paymentRes = activeChannel.SendMonoHopUnidirectionalPayment amount
            match paymentRes with
            | Error sendMonoHopPaymentError ->
                if sendMonoHopPaymentError.PossibleBug then
                    let msg =
                        SPrintF2
                            "error sending monohop payment on channel %s: %s"
                            (channelId.ToString())
                            sendMonoHopPaymentError.Message
                    Infrastructure.ReportWarningMessage msg
                return Error <| NodeSendMonoHopPaymentError.SendPayment sendMonoHopPaymentError
            | Ok activeChannelAfterPayment ->
                (activeChannelAfterPayment :> IDisposable).Dispose()
                return Ok ()
    }

    member this.ReceiveMonoHopPayment (channelId: ChannelId)
                                          : Async<Result<unit, NodeReceiveMonoHopPaymentError>> = async {
        let! activeChannelRes = ActiveChannel.AcceptReestablish this.ChannelStore this.TransportListener channelId
        match activeChannelRes with
        | Error reconnectActiveChannelError ->
            if reconnectActiveChannelError.PossibleBug then
                let msg =
                    SPrintF2
                        "error accepting connection from peer to receive monohop payment on channel %s: %s"
                        (channelId.ToString())
                        reconnectActiveChannelError.Message
                Infrastructure.ReportWarningMessage msg
            return Error <| NodeReceiveMonoHopPaymentError.Reconnect reconnectActiveChannelError
        | Ok activeChannel ->
            let! paymentRes = activeChannel.RecvMonoHopUnidirectionalPayment()
            match paymentRes with
            | Error recvMonoHopPaymentError ->
                if recvMonoHopPaymentError.PossibleBug then
                    let msg =
                        SPrintF2
                            "error accepting monohop payment on channel %s: %s"
                            (channelId.ToString())
                            recvMonoHopPaymentError.Message
                    Infrastructure.ReportWarningMessage msg
                return Error <| NodeReceiveMonoHopPaymentError.ReceivePayment recvMonoHopPaymentError
            | Ok activeChannelAfterPaymentReceived ->
                (activeChannelAfterPaymentReceived :> IDisposable).Dispose()
                return Ok ()
    }

    member this.LockChannelFunding (channelId: ChannelId)
                                       : Async<Result<unit, ReconnectActiveChannelError>> =
        async {
            let channelInfo = this.ChannelStore.ChannelInfo channelId
            let! activeChannelRes =
                if channelInfo.IsFunder then
                    ActiveChannel.ConnectReestablish this.ChannelStore this.SecretKey channelId
                else
                    ActiveChannel.AcceptReestablish this.ChannelStore this.TransportListener channelId
            match activeChannelRes with
            | Error reconnectActiveChannelError ->
                if reconnectActiveChannelError.PossibleBug then
                    let msg =
                        SPrintF2
                            "error reconnecting to peer to lock channel %s: %s"
                            (channelId.ToString())
                            reconnectActiveChannelError.Message
                    Infrastructure.ReportWarningMessage msg
                return Error reconnectActiveChannelError
            | Ok activeChannel ->
                (activeChannel :> IDisposable).Dispose()
                return Ok ()
        }

module LightningNode =
    let Start = LightningNode.Start
    let OpenChannel (lightningNode: LightningNode) = lightningNode.OpenChannel
    let AcceptChannel (lightningNode: LightningNode) = lightningNode.AcceptChannel ()
    let SendMonoHopPayment (lightningNode: LightningNode) = lightningNode.SendMonoHopPayment
    let ReceiveMonoHopPayment (lightningNode: LightningNode) = lightningNode.ReceiveMonoHopPayment
    let LockChannelFunding (lightningNode: LightningNode) = lightningNode.LockChannelFunding
    let LnEndPoint (lightningNode: LightningNode) = lightningNode.LnEndPoint

