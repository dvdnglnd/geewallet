namespace GWallet.Backend.UtxoCoin.Lightning

open System

[<AutoOpen>]
module Primitives =
    type NodeId internal (dnlNodeId: DotNetLightning.Utils.Primitives.NodeId) =
        member internal this.DnlNodeId = dnlNodeId

    type ChannelId internal (dnlChannelId: DotNetLightning.Utils.Primitives.ChannelId) =
        static member NewRandom(): ChannelId =
            let random = Org.BouncyCastle.Security.SecureRandom() :> Random
            let temporaryChannelIdBytes: array<byte> = Array.zeroCreate 32
            random.NextBytes temporaryChannelIdBytes
            temporaryChannelIdBytes
            |> NBitcoin.uint256
            |> DotNetLightning.Utils.Primitives.ChannelId
            |> ChannelId

        static member Parse (s: string): Option<ChannelId> =
            try
                s
                |> NBitcoin.uint256
                |> DotNetLightning.Utils.Primitives.ChannelId
                |> ChannelId
                |> Some
            with
            | :? FormatException -> None

        member internal this.DnlChannelId = dnlChannelId

        override this.ToString() = this.DnlChannelId.Value.ToString()

    type internal HTLCId = DotNetLightning.Utils.Primitives.HTLCId
    type internal FeeRatePerKw = DotNetLightning.Utils.Primitives.FeeRatePerKw
    type internal LNMoney = DotNetLightning.Utils.LNMoney
    type internal BlockHeight = DotNetLightning.Utils.Primitives.BlockHeight
    type internal BlockHeightOffset16 = DotNetLightning.Utils.Primitives.BlockHeightOffset16
    type internal BlockHeightOffset32 = DotNetLightning.Utils.Primitives.BlockHeightOffset32
    type internal TxId = DotNetLightning.Utils.Primitives.TxId
    type internal PeerId = DotNetLightning.Utils.Primitives.PeerId
    type internal TxOutIndex = DotNetLightning.Utils.Primitives.TxOutIndex
    type internal TxIndexInBlock = DotNetLightning.Utils.Primitives.TxIndexInBlock

    let internal BlockHeight = DotNetLightning.Utils.Primitives.BlockHeight
    let internal BlockHeightOffset16 = DotNetLightning.Utils.Primitives.BlockHeightOffset16
    let internal BlockHeightOffset32 = DotNetLightning.Utils.Primitives.BlockHeightOffset32
    let internal LNMoney = DotNetLightning.Utils.LNMoney
    let internal TxId = DotNetLightning.Utils.Primitives.TxId
    let internal PeerId = DotNetLightning.Utils.Primitives.PeerId
    let internal TxOutIndex = DotNetLightning.Utils.Primitives.TxOutIndex
    let internal TxIndexInBlock = DotNetLightning.Utils.Primitives.TxIndexInBlock

