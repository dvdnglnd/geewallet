namespace GWallet.Backend.UtxoCoin.Lightning

open System
open System.IO
open System.Net

open NBitcoin
open DotNetLightning.Utils

open GWallet.Backend
open GWallet.Backend.UtxoCoin
open GWallet.Backend.FSharpUtil.UwpHacks

type internal NodeOpenChannelError =
    | Connect of ConnectError
    | OpenChannel of OpenChannelError
    interface IErrorMsg with
        member self.Message =
            match self with
            | Connect connectError ->
                SPrintF1 "error connecting: %s" (connectError :> IErrorMsg).Message
            | OpenChannel openChannelError ->
                SPrintF1 "error opening channel: %s" (openChannelError :> IErrorMsg).Message

type internal NodeAcceptChannelError =
    | AcceptPeer of ConnectError
    | AcceptChannel of AcceptChannelError
    interface IErrorMsg with
        member self.Message =
            match self with
            | AcceptPeer connectError ->
                SPrintF1 "error accepting connection: %s" (connectError :> IErrorMsg).Message
            | AcceptChannel acceptChannelError ->
                SPrintF1 "error accepting channel: %s" (acceptChannelError :> IErrorMsg).Message

type internal NodeSendMonoHopPaymentError =
    | Reconnect of ReconnectActiveChannelError
    | SendPayment of SendMonoHopPaymentError
    interface IErrorMsg with
        member self.Message =
            match self with
            | Reconnect reconnectActiveChannelError ->
                SPrintF1 "error reconnecting channel: %s" (reconnectActiveChannelError :> IErrorMsg).Message
            | SendPayment sendMonoHopPaymentError ->
                SPrintF1 "error sending payment on reconnected channel: %s"
                         (sendMonoHopPaymentError :> IErrorMsg).Message

type internal NodeReceiveMonoHopPaymentError =
    | Reconnect of ReconnectActiveChannelError
    | ReceivePayment of RecvMonoHopPaymentError
    interface IErrorMsg with
        member self.Message =
            match self with
            | Reconnect reconnectActiveChannelError ->
                SPrintF1 "error reconnecting channel: %s" (reconnectActiveChannelError :> IErrorMsg).Message
            | ReceivePayment recvMonoHopPaymentError ->
                SPrintF1 "error receiving payment on reconnected channel: %s"
                         (recvMonoHopPaymentError :> IErrorMsg).Message

type internal NodeAcceptCloseChannelError =
    | Reconnect of ReconnectActiveChannelError
    | AcceptCloseChannel of CloseChannelError
    interface IErrorMsg with
        member self.Message =
            match self with
            | Reconnect reconnectActiveChannelError ->
                SPrintF1 "error reconnecting channel: %s" (reconnectActiveChannelError :> IErrorMsg).Message
            | AcceptCloseChannel acceptCloseChannelError ->
                SPrintF1 "error accepting channel close on reconnected channel: %s"
                         (acceptCloseChannelError :> IErrorMsg).Message

type internal NodeReceiveLightningEventError =
    | Reconnect of ReconnectActiveChannelError
    interface IErrorMsg with
        member self.Message =
            match self with
            | Reconnect reconnectActiveChannelError ->
                SPrintF1 "error reconnecting channel: %s" (reconnectActiveChannelError :> IErrorMsg).Message

type IChannelToBeOpened =
    abstract member ConfirmationsRequired: uint32 with get
    abstract member ChannelId: ChannelIdentifier with get

type IncomingChannelEvent =
    | MonoHopUnidirectionalPayment
    | Shutdown

type PendingChannel internal (outgoingUnfundedChannel: OutgoingUnfundedChannel) =
    member internal self.OutgoingUnfundedChannel = outgoingUnfundedChannel
    member public self.Accept (): Async<Result<TransactionIdentifier, IErrorMsg>> = async {
        let! fundedChannelRes =
            FundedChannel.FundChannel self.OutgoingUnfundedChannel
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
        member self.ChannelId
            with get(): ChannelIdentifier =
                self.OutgoingUnfundedChannel.ChannelId

type Node internal (channelStore: ChannelStore, transportListener: TransportListener) =
    member val ChannelStore = channelStore
    member val internal TransportListener = transportListener
    member val internal SecretKey = transportListener.NodeSecret
    member val internal NodeId = transportListener.NodeId
    member val EndPoint = transportListener.EndPoint
    member val Account = channelStore.Account

    interface IDisposable with
        member self.Dispose() =
            (self.TransportListener :> IDisposable).Dispose()

    member internal self.OpenChannel (nodeEndPoint: NodeEndPoint)
                                     (channelCapacity: TransferAmount)
                                     (metadata: TransactionMetadata)
                                     (password: string)
                                         : Async<Result<PendingChannel, IErrorMsg>> = async {
        let peerId = PeerId (nodeEndPoint.IPEndPoint :> EndPoint)
        let nodeId = nodeEndPoint.NodeId.ToString() |> NBitcoin.PubKey |> NodeId
        let! connectRes = PeerNode.Connect self.SecretKey nodeId peerId
        match connectRes with
        | Error connectError ->
            if connectError.PossibleBug then
                let msg =
                    SPrintF3
                        "error connecting to peer %s to open channel of capacity %M: %s"
                        (nodeEndPoint.ToString())
                        channelCapacity.ValueToSend
                        (connectError :> IErrorMsg).Message
                Infrastructure.ReportWarningMessage msg
            return Error <| (NodeOpenChannelError.Connect connectError :> IErrorMsg)
        | Ok peerNode ->
            let! outgoingUnfundedChannelRes =
                OutgoingUnfundedChannel.OpenChannel
                    peerNode
                    self.Account
                    channelCapacity
                    metadata
                    password
            match outgoingUnfundedChannelRes with
            | Error openChannelError ->
                if openChannelError.PossibleBug then
                    let msg =
                        SPrintF3
                            "error opening channel with peer %s of capacity %M: %s"
                            (nodeEndPoint.ToString())
                            channelCapacity.ValueToSend
                            (openChannelError :> IErrorMsg).Message
                    Infrastructure.ReportWarningMessage msg
                return Error <| (NodeOpenChannelError.OpenChannel openChannelError :> IErrorMsg)
            | Ok outgoingUnfundedChannel ->
                return Ok <| PendingChannel(outgoingUnfundedChannel)
    }

    member internal self.AcceptChannel (): Async<Result<TransactionIdentifier, IErrorMsg>> = async {
        let! acceptPeerRes =
            PeerNode.AcceptAnyFromTransportListener self.TransportListener
        match acceptPeerRes with
        | Error connectError ->
            if connectError.PossibleBug then
                let msg =
                    SPrintF1
                        "error accepting connection from peer to accept channel: %s"
                        (connectError :> IErrorMsg).Message
                Infrastructure.ReportWarningMessage msg
            return Error <| (NodeAcceptChannelError.AcceptPeer connectError :> IErrorMsg)
        | Ok peerNode ->
            let! fundedChannelRes = FundedChannel.AcceptChannel peerNode self.Account
            match fundedChannelRes with
            | Error acceptChannelError ->
                if acceptChannelError.PossibleBug then
                    let msg =
                        SPrintF2
                            "error accepting channel from peer %s: %s"
                            (peerNode.NodeEndPoint.ToString())
                            (acceptChannelError :> IErrorMsg).Message
                    Infrastructure.ReportWarningMessage msg
                return Error <| (NodeAcceptChannelError.AcceptChannel acceptChannelError :> IErrorMsg)
            | Ok fundedChannel ->
                let txId = fundedChannel.FundingTxId
                (fundedChannel :> IDisposable).Dispose()
                return Ok txId
    }

    member internal self.SendMonoHopPayment (channelId: ChannelIdentifier)
                                            (transferAmount: TransferAmount)
                                                : Async<Result<unit, IErrorMsg>> = async {
        let amount =
            let btcAmount = transferAmount.ValueToSend
            let lnAmount = int64(btcAmount * decimal DotNetLightning.Utils.LNMoneyUnit.BTC)
            DotNetLightning.Utils.LNMoney lnAmount
        let! activeChannelRes = ActiveChannel.ConnectReestablish self.ChannelStore self.SecretKey channelId
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

    member internal self.ReceiveMonoHopPayment (channelId: ChannelIdentifier)
                                                   : Async<Result<unit, IErrorMsg>> = async {
        let! activeChannelRes = ActiveChannel.AcceptReestablish self.ChannelStore self.TransportListener channelId
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
            let connectedChannel = activeChannel.ConnectedChannel

            Infrastructure.LogDebug "Waiting for lightning message"
            let! recvChannelMsgRes = connectedChannel.PeerNode.RecvChannelMsg()
            match recvChannelMsgRes with
            | Error err ->
                return failwith <| SPrintF1 "Received error while waiting for lightning message: %s" (err :> IErrorMsg).Message
            | Ok (_, channelMsg) ->
                match channelMsg with
                | :? DotNetLightning.Serialize.Msgs.MonoHopUnidirectionalPaymentMsg as monoHopUnidirectionalPaymentMsg ->
                    return! self.HandleMonoHopUnidirectionalPaymentMsg activeChannel channelId monoHopUnidirectionalPaymentMsg
                | msg ->
                    return failwith <| SPrintF1 "Unexpected msg while waiting for monohop payment message: %As" msg
    }

    member private self.HandleMonoHopUnidirectionalPaymentMsg (activeChannel: ActiveChannel)
                                                              (channelId: ChannelIdentifier)
                                                              (monoHopUnidirectionalPaymentMsg: DotNetLightning.Serialize.Msgs.MonoHopUnidirectionalPaymentMsg)
                                                                  : Async<Result<unit, IErrorMsg>> = async {
        let! paymentRes = activeChannel.RecvMonoHopUnidirectionalPayment monoHopUnidirectionalPaymentMsg
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

    member internal self.InitiateCloseChannel (channelId: ChannelIdentifier): Async<Result<unit, IErrorMsg>> =
        async {
            let! connectRes = ActiveChannel.ConnectReestablish self.ChannelStore self.SecretKey channelId
            match connectRes with
            | Error connectError ->
                return failwith <| SPrintF1 "Error reestablishing channel: %s" (connectError :> IErrorMsg).Message
            | Ok activeChannel ->
                let! closeRes = ClosedChannel.InitiateCloseChannel activeChannel.ConnectedChannel
                match closeRes with
                | Error closeError ->
                    return failwith <| SPrintF1 "Error closing channel: %s" (closeError :> IErrorMsg).Message
                | Ok _ ->
                    return Ok ()
        }

    member internal self.AcceptCloseChannel (channelId: ChannelIdentifier)
                                                : Async<Result<unit, IErrorMsg>> = async {
        let! activeChannelRes = ActiveChannel.AcceptReestablish self.ChannelStore self.TransportListener channelId
        match activeChannelRes with
        | Error reconnectActiveChannelError ->
            if reconnectActiveChannelError.PossibleBug then
                let msg =
                    SPrintF2
                        "error accepting connection from peer to accept close channel on channel %s: %s"
                        (channelId.ToString())
                        (reconnectActiveChannelError :> IErrorMsg).Message
                Infrastructure.ReportWarningMessage msg
            return Error <| (NodeAcceptCloseChannelError.Reconnect reconnectActiveChannelError :> IErrorMsg)
        | Ok activeChannel ->
            let connectedChannel = activeChannel.ConnectedChannel

            Infrastructure.LogDebug "Waiting for lightning message"
            let! recvChannelMsgRes = connectedChannel.PeerNode.RecvChannelMsg()
            match recvChannelMsgRes with
            | Error err ->
                return failwith <| SPrintF1 "Received error while waiting for lightning message: %s" (err :> IErrorMsg).Message
            | Ok (_, channelMsg) ->
                match channelMsg with
                | :? DotNetLightning.Serialize.Msgs.ShutdownMsg as shutdownMsg ->
                    return! self.HandleShutdownMsg activeChannel shutdownMsg
                | msg ->
                    return failwith <| SPrintF1 "Unexpected msg while waiting for shutdown message: %As" msg
    }

    member private self.HandleShutdownMsg (activeChannel: ActiveChannel)
                                          (shutdownMsg: DotNetLightning.Serialize.Msgs.ShutdownMsg)
                                              : Async<Result<unit, IErrorMsg>> = async {
        let! closeRes = ClosedChannel.AcceptCloseChannel (activeChannel.ConnectedChannel, shutdownMsg)
        match closeRes with
        | Error acceptCloseChannelError ->
            return Error <| (NodeAcceptCloseChannelError.AcceptCloseChannel acceptCloseChannelError :> IErrorMsg)
        | Ok _ ->
            return Ok ()
    }

    member internal self.LockChannelFunding (channelId: ChannelIdentifier)
                                                : Async<Result<unit, IErrorMsg>> =
        async {
            let channelInfo = self.ChannelStore.ChannelInfo channelId
            let! activeChannelRes =
                if channelInfo.IsFunder then
                    ActiveChannel.ConnectReestablish self.ChannelStore self.SecretKey channelId
                else
                    ActiveChannel.AcceptReestablish self.ChannelStore self.TransportListener channelId
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

    member internal self.ReceiveLightningEvent (channelId: ChannelIdentifier)
                                            : Async<Result<IncomingChannelEvent, IErrorMsg>> =
        async {
            let! activeChannelRes = ActiveChannel.AcceptReestablish self.ChannelStore self.TransportListener channelId
            match activeChannelRes with
            | Error reconnectActiveChannelError ->
                if reconnectActiveChannelError.PossibleBug then
                    let msg =
                        SPrintF2
                            "error accepting connection from peer to receive lightning event on channel %s: %s"
                            (channelId.ToString())
                            (reconnectActiveChannelError :> IErrorMsg).Message
                    Infrastructure.ReportWarningMessage msg
                return Error <| (NodeReceiveLightningEventError.Reconnect reconnectActiveChannelError :> IErrorMsg)
            | Ok activeChannel ->
                let connectedChannel = activeChannel.ConnectedChannel
                Infrastructure.LogDebug "Waiting for lightning message"
                let! recvChannelMsgRes = connectedChannel.PeerNode.RecvChannelMsg()
                match recvChannelMsgRes with
                | Error err ->
                    return failwith <| SPrintF1 "Received error while waiting for lightning message: %s" (err :> IErrorMsg).Message
                | Ok (_, channelMsg) ->
                    match channelMsg with
                    | :? DotNetLightning.Serialize.Msgs.MonoHopUnidirectionalPaymentMsg as monoHopUnidirectionalPaymentMsg ->
                        let! res = self.HandleMonoHopUnidirectionalPaymentMsg activeChannel channelId monoHopUnidirectionalPaymentMsg
                        match res with
                        | Error err -> return Error err
                        | Ok _ -> return Ok IncomingChannelEvent.MonoHopUnidirectionalPayment
                    | :? DotNetLightning.Serialize.Msgs.ShutdownMsg as shutdownMsg ->
                        let! res = self.HandleShutdownMsg activeChannel shutdownMsg
                        match res with
                        | Error err -> return Error err
                        | Ok _ -> return Ok IncomingChannelEvent.Shutdown
                    | msg -> return failwith <| SPrintF1 "Received invalid msg while waiting for lightning event: %A" msg
        }

module public Connection =
    let public Start (channelStore: ChannelStore)
                     (password: string)
                     (bindAddress: IPEndPoint)
                         : Node =
        let secretKey: ExtKey =
            let privateKey = Account.GetPrivateKey channelStore.Account password
            //let bytes: array<byte> = Array.zeroCreate Key.BytesLength
            let bytes: array<byte> = Array.zeroCreate 32
            use bytesStream = new MemoryStream(bytes)
            let stream = NBitcoin.BitcoinStream(bytesStream, true)
            privateKey.ReadWrite stream
            NBitcoin.ExtKey bytes
        let transportListener = TransportListener.Bind secretKey bindAddress
        new Node (channelStore, transportListener)

