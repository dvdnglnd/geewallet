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

type internal NodeOpenChannelError =
    | Connect of ConnectError
    | OpenChannel of OpenChannelError
    interface IErrorMsg with
        member this.Message =
            match this with
            | Connect connectError ->
                SPrintF1 "error connecting: %s" (connectError :> IErrorMsg).Message
            | OpenChannel openChannelError ->
                SPrintF1 "error opening channel: %s" openChannelError.Message

type internal NodeAcceptChannelError =
    | AcceptPeer of ConnectError
    | AcceptChannel of AcceptChannelError
    interface IErrorMsg with
        member this.Message =
            match this with
            | AcceptPeer connectError ->
                SPrintF1 "error accepting connection: %s" (connectError :> IErrorMsg).Message
            | AcceptChannel acceptChannelError ->
                SPrintF1 "error accepting channel: %s" (acceptChannelError :> IErrorMsg).Message

type internal NodeSendMonoHopPaymentError =
    | Reconnect of ReconnectActiveChannelError
    | SendPayment of SendMonoHopPaymentError
    interface IErrorMsg with
        member this.Message =
            match this with
            | Reconnect reconnectActiveChannelError ->
                SPrintF1 "error reconnecting channel: %s" (reconnectActiveChannelError :> IErrorMsg).Message
            | SendPayment sendMonoHopPaymentError ->
                SPrintF1 "error sending payment on reconnected channel: %s"
                         (sendMonoHopPaymentError :> IErrorMsg).Message

type internal NodeReceiveMonoHopPaymentError =
    | Reconnect of ReconnectActiveChannelError
    | ReceivePayment of RecvMonoHopPaymentError
    interface IErrorMsg with
        member this.Message =
            match this with
            | Reconnect reconnectActiveChannelError ->
                SPrintF1 "error reconnecting channel: %s" (reconnectActiveChannelError :> IErrorMsg).Message
            | ReceivePayment recvMonoHopPaymentError ->
                SPrintF1 "error receiving payment on reconnected channel: %s"
                         (recvMonoHopPaymentError :> IErrorMsg).Message

type IChannelToBeOpened =
    abstract member ConfirmationsRequired: uint32 with get

type PendingChannel internal (outgoingUnfundedChannel: OutgoingUnfundedChannel) =
    member internal this.OutgoingUnfundedChannel = outgoingUnfundedChannel
    member public this.Accept (): Async<Result<TxIdWrapper, IErrorMsg>> = async {
        let! fundedChannelRes =
            FundedChannel.FundChannel this.OutgoingUnfundedChannel
        match fundedChannelRes with
        | Error fundChannelError ->
            if fundChannelError.PossibleBug then
                let msg = SPrintF1 "Error funding channel: %s" (fundChannelError :> IErrorMsg).Message
                Infrastructure.ReportWarningMessage msg
            return Error (fundChannelError :> IErrorMsg)
        | Ok fundedChannel ->
            let txId = fundedChannel.FundingTxId
            (fundedChannel :> IDisposable).Dispose()
            return Ok txId
    }
    interface IChannelToBeOpened with
        member self.ConfirmationsRequired
            with get(): uint32 =
                self.OutgoingUnfundedChannel.MinimumDepth.Value

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
    member internal this.TransportListener = transportListener
    member internal this.SecretKey = this.TransportListener.NodeSecret
    member this.NodeId = this.TransportListener.NodeId
    member this.LnEndPoint = this.TransportListener.LnEndPoint
    member this.Account = this.ChannelStore.Account

    interface IDisposable with
        member this.Dispose() =
            (this.TransportListener :> IDisposable).Dispose()

    member internal this.OpenChannel (lnEndPoint: LnEndPoint)
                            (channelCapacity: TransferAmount)
                            (metadata: TransactionMetadata)
                            (password: string)
                                : Async<Result<PendingChannel, IErrorMsg>> = async {
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
                        (connectError :> IErrorMsg).Message
                Infrastructure.ReportWarningMessage msg
            return Error <| (NodeOpenChannelError.Connect connectError :> IErrorMsg)
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
                return Error <| (NodeOpenChannelError.OpenChannel openChannelError :> IErrorMsg)
            | Ok outgoingUnfundedChannel ->
                return Ok <| PendingChannel(outgoingUnfundedChannel)
    }

    member internal this.AcceptChannel (): Async<Result<TxIdWrapper, IErrorMsg>> = async {
        let! acceptPeerRes =
            PeerWrapper.AcceptAnyFromTransportListener this.TransportListener
        match acceptPeerRes with
        | Error connectError ->
            if connectError.PossibleBug then
                let msg =
                    SPrintF1
                        "error accepting connection from peer to accept channel: %s"
                        (connectError :> IErrorMsg).Message
                Infrastructure.ReportWarningMessage msg
            return Error <| (NodeAcceptChannelError.AcceptPeer connectError :> IErrorMsg)
        | Ok peerWrapper ->
            let! fundedChannelRes = FundedChannel.AcceptChannel peerWrapper this.Account
            match fundedChannelRes with
            | Error acceptChannelError ->
                if acceptChannelError.PossibleBug then
                    let msg =
                        SPrintF2
                            "error accepting channel from peer %s: %s"
                            (peerWrapper.LnEndPoint.ToString())
                            (acceptChannelError :> IErrorMsg).Message
                    Infrastructure.ReportWarningMessage msg
                return Error <| (NodeAcceptChannelError.AcceptChannel acceptChannelError :> IErrorMsg)
            | Ok fundedChannel ->
                let txId = fundedChannel.FundingTxId
                (fundedChannel :> IDisposable).Dispose()
                return Ok txId
    }

    member internal this.SendMonoHopPayment (channelId: ChannelIdWrapper)
                                            (transferAmount: TransferAmount)
                                                : Async<Result<unit, IErrorMsg>> = async {
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
                        (reconnectActiveChannelError :> IErrorMsg).Message
                Infrastructure.ReportWarningMessage msg
            return Error <| (NodeSendMonoHopPaymentError.Reconnect reconnectActiveChannelError :> IErrorMsg)
        | Ok activeChannel ->
            let! paymentRes = activeChannel.SendMonoHopUnidirectionalPayment amount
            match paymentRes with
            | Error sendMonoHopPaymentError ->
                if sendMonoHopPaymentError.PossibleBug then
                    let msg =
                        SPrintF2
                            "error sending monohop payment on channel %s: %s"
                            (channelId.ToString())
                            (sendMonoHopPaymentError :> IErrorMsg).Message
                    Infrastructure.ReportWarningMessage msg
                return Error <| (NodeSendMonoHopPaymentError.SendPayment sendMonoHopPaymentError :> IErrorMsg)
            | Ok activeChannelAfterPayment ->
                (activeChannelAfterPayment :> IDisposable).Dispose()
                return Ok ()
    }

    member internal this.ReceiveMonoHopPayment (channelId: ChannelIdWrapper)
                                          : Async<Result<unit, IErrorMsg>> = async {
        let! activeChannelRes = ActiveChannel.AcceptReestablish this.ChannelStore this.TransportListener channelId
        match activeChannelRes with
        | Error reconnectActiveChannelError ->
            if reconnectActiveChannelError.PossibleBug then
                let msg =
                    SPrintF2
                        "error accepting connection from peer to receive monohop payment on channel %s: %s"
                        (channelId.ToString())
                        (reconnectActiveChannelError :> IErrorMsg).Message
                Infrastructure.ReportWarningMessage msg
            return Error <| (NodeReceiveMonoHopPaymentError.Reconnect reconnectActiveChannelError :> IErrorMsg)
        | Ok activeChannel ->
            let! paymentRes = activeChannel.RecvMonoHopUnidirectionalPayment()
            match paymentRes with
            | Error recvMonoHopPaymentError ->
                if recvMonoHopPaymentError.PossibleBug then
                    let msg =
                        SPrintF2
                            "error accepting monohop payment on channel %s: %s"
                            (channelId.ToString())
                            (recvMonoHopPaymentError :> IErrorMsg).Message
                    Infrastructure.ReportWarningMessage msg
                return Error <| (NodeReceiveMonoHopPaymentError.ReceivePayment recvMonoHopPaymentError :> IErrorMsg)
            | Ok activeChannelAfterPaymentReceived ->
                (activeChannelAfterPaymentReceived :> IDisposable).Dispose()
                return Ok ()
    }

    member internal this.LockChannelFunding (channelId: ChannelIdWrapper)
                                                : Async<Result<unit, IErrorMsg>> =
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
                            (reconnectActiveChannelError :> IErrorMsg).Message
                    Infrastructure.ReportWarningMessage msg
                return Error (reconnectActiveChannelError :> IErrorMsg)
            | Ok activeChannel ->
                (activeChannel :> IDisposable).Dispose()
                return Ok ()
        }

module public Lightning =
    let public Start = LightningNode.Start
    let public OpenChannel (lightningNode: LightningNode) = lightningNode.OpenChannel
    let public AcceptChannel (lightningNode: LightningNode) = lightningNode.AcceptChannel ()
    let public SendMonoHopPayment (lightningNode: LightningNode) = lightningNode.SendMonoHopPayment
    let public ReceiveMonoHopPayment (lightningNode: LightningNode) = lightningNode.ReceiveMonoHopPayment
    let public LockChannelFunding (lightningNode: LightningNode) = lightningNode.LockChannelFunding
    let public LnEndPoint (lightningNode: LightningNode) = lightningNode.LnEndPoint

