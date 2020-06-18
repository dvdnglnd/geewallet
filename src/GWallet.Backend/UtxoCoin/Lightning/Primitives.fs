namespace GWallet.Backend.UtxoCoin.Lightning

open System
open System.Net

open GWallet.Backend.FSharpUtil.UwpHacks

(*
type BitcoinPrivateKey internal (key: Key) =
    member internal this.NBitcoinKey = key

type BitcoinPublicKey internal (pubKey: PubKey) =
    member internal this.NBitcoinPubKey = pubKey
*)

type NodeId internal (dnlNodeId: DotNetLightning.Utils.Primitives.NodeId) =
    member internal this.DnlNodeId =
        dnlNodeId

    static member internal FromDnl (dnlNodeId: DotNetLightning.Utils.Primitives.NodeId) =
        NodeId dnlNodeId

    static member internal FromPubKey (pubKey: NBitcoin.PubKey): NodeId =
        NodeId (DotNetLightning.Utils.Primitives.NodeId pubKey)

    static member Parse (text: string): NodeId =
        text
        |> NBitcoin.PubKey
        |> DotNetLightning.Utils.Primitives.NodeId
        |> NodeId

    override this.ToString() =
        this.DnlNodeId.Value.ToString()

module NodeId =
    let Parse = NodeId.Parse

(*
type NodeId private (pubKeyString: string) =
    static member Parse (pubKeyString: string): NodeId =
        pubKeyString
        |> NBitcoin.PubKey
        |> ignore
        NodeId pubKeyString

    override this.ToString() = pubKeyString

    static member internal FromDnl (dnlNodeId: DotNetLightning.Utils.Primitives.NodeId) =
        NodeId(dnlNodeId.Value.ToString())

    member internal this.DnlNodeId =
        this.ToString()
        |> NBitcoin.PubKey
        |> DotNetLightning.Utils.Primitives.NodeId
*)

type LnEndPoint = internal {
    NodeId: NodeId
    IPEndPoint: IPEndPoint
} with
    static member Parse (text: string): LnEndPoint =
        let atIndex = text.IndexOf "@"
        if atIndex = -1 then
            raise <| FormatException "No '@' in endpoint string"
        let nodeIdText = text.[..atIndex - 1]
        let ipEndPointText = text.[atIndex + 1 ..]

        let portSeparatorIndex = ipEndPointText.LastIndexOf ':'
        if portSeparatorIndex = -1 then
            raise <| FormatException "No ':' after '@' in endpoint string"
        let ipAddressText = ipEndPointText.[..portSeparatorIndex - 1]
        let portText = ipEndPointText.[portSeparatorIndex + 1 ..]

        let nodeId = NodeId.Parse nodeIdText
        let ipAddress = IPAddress.Parse ipAddressText
        let port = UInt16.Parse portText
        {
            NodeId = nodeId
            IPEndPoint = IPEndPoint(ipAddress, int port)
        }

    static member FromParts (nodeId: NodeId) (ipEndPoint: IPEndPoint) =
        {
            NodeId = nodeId
            IPEndPoint = ipEndPoint
        }

    override this.ToString() =
        SPrintF2 "%s@%s" (this.NodeId.ToString()) (this.IPEndPoint.ToString())

type ChannelId = internal {
    DnlChannelId: DotNetLightning.Utils.Primitives.ChannelId
} with
    static member internal FromDnl (dnlChannelId: DotNetLightning.Utils.Primitives.ChannelId): ChannelId =
        { DnlChannelId = dnlChannelId }

    static member NewRandom(): ChannelId =
        let dnlChannelId =
            let random = Org.BouncyCastle.Security.SecureRandom() :> Random
            let temporaryChannelIdBytes: array<byte> = Array.zeroCreate 32
            random.NextBytes temporaryChannelIdBytes
            temporaryChannelIdBytes
            |> NBitcoin.uint256
            |> DotNetLightning.Utils.Primitives.ChannelId
        { DnlChannelId = dnlChannelId }

    static member Parse (text: string): Option<ChannelId> =
        try
            let dnlChannelId =
                text
                |> NBitcoin.uint256
                |> DotNetLightning.Utils.Primitives.ChannelId
            Some { DnlChannelId = dnlChannelId }
        with
        | :? FormatException -> None

    override this.ToString() = this.DnlChannelId.Value.ToString()

module ChannelId =
    let ToString (channelId: ChannelId): string = channelId.ToString()

type TxId = internal {
    DnlTxId: DotNetLightning.Utils.Primitives.TxId
} with
    (*
    static member internal FromDnl (dnlTxId: DotNetLightning.Utils.Primitives.TxId)
                                       : TxId =
        { DnlTxId = dnlTxId }
    *)

    static member internal FromHash (txIdHash: NBitcoin.uint256): TxId =
        { DnlTxId = DotNetLightning.Utils.Primitives.TxId txIdHash }

    override this.ToString() = this.DnlTxId.Value.ToString()

module TxId =
    let FromDnl (dnlTxId: DotNetLightning.Utils.Primitives.TxId): TxId =
        { DnlTxId = dnlTxId }
    let ToString (txId: TxId) = txId.ToString()

type internal HTLCId = DotNetLightning.Utils.Primitives.HTLCId
type internal FeeRatePerKw = DotNetLightning.Utils.Primitives.FeeRatePerKw
type internal LNMoney = DotNetLightning.Utils.LNMoney
type internal BlockHeight = DotNetLightning.Utils.Primitives.BlockHeight
type internal BlockHeightOffset16 = DotNetLightning.Utils.Primitives.BlockHeightOffset16
type internal BlockHeightOffset32 = DotNetLightning.Utils.Primitives.BlockHeightOffset32
type internal PeerId = DotNetLightning.Utils.Primitives.PeerId
type internal TxOutIndex = DotNetLightning.Utils.Primitives.TxOutIndex
type internal TxIndexInBlock = DotNetLightning.Utils.Primitives.TxIndexInBlock

[<AutoOpen>]
module Constructors =
    let internal BlockHeight = DotNetLightning.Utils.Primitives.BlockHeight
    let internal BlockHeightOffset16 = DotNetLightning.Utils.Primitives.BlockHeightOffset16
    let internal BlockHeightOffset32 = DotNetLightning.Utils.Primitives.BlockHeightOffset32
    let internal LNMoney = DotNetLightning.Utils.LNMoney
    let internal PeerId = DotNetLightning.Utils.Primitives.PeerId
    let internal TxOutIndex = DotNetLightning.Utils.Primitives.TxOutIndex
    let internal TxIndexInBlock = DotNetLightning.Utils.Primitives.TxIndexInBlock

