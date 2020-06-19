namespace GWallet.Backend.UtxoCoin.Lightning

open System
open System.IO
open System.Net

open NBitcoin
open DotNetLightning.Channel
open DotNetLightning.Chain
open DotNetLightning.Utils
open DotNetLightning.Crypto
open DotNetLightning.Transactions
open DotNetLightning.Serialize

open GWallet.Backend
open GWallet.Backend.FSharpUtil
open GWallet.Backend.FSharpUtil.UwpHacks
open GWallet.Backend.UtxoCoin.Lightning.Util
open GWallet.Backend.UtxoCoin.Lightning.Primitives

open Newtonsoft.Json

type SerializedCommitmentSpec = {
    HTLCs: Map<HTLCId, DirectedHTLC>
    FeeRatePerKw: FeeRatePerKw
    ToLocal: LNMoney
    ToRemote: LNMoney
}

type SerializedCommitments = {
    ChannelId: ChannelId
    ChannelFlags: uint8
    FundingScriptCoin: ScriptCoin
    LocalChanges: LocalChanges
    LocalCommit: LocalCommit
    LocalNextHTLCId: HTLCId
    LocalParams: LocalParams
    OriginChannels: Map<HTLCId, HTLCSource>
    RemoteChanges: RemoteChanges
    RemoteCommit: RemoteCommit
    RemoteNextCommitInfo: RemoteNextCommitInfo
    RemoteNextHTLCId: HTLCId
    RemoteParams: RemoteParams
    RemotePerCommitmentSecrets: ShaChain
}

type private CommitmentsJsonConverter() =
    inherit JsonConverter<Commitments>()

    override this.ReadJson(reader: JsonReader, _: Type, _: Commitments, _: bool, serializer: JsonSerializer) =
        let serializedCommitments = serializer.Deserialize<SerializedCommitments> reader
        let commitments: Commitments = {
            RemotePerCommitmentSecrets = serializedCommitments.RemotePerCommitmentSecrets
            RemoteParams = serializedCommitments.RemoteParams
            RemoteNextHTLCId = serializedCommitments.RemoteNextHTLCId
            RemoteNextCommitInfo = serializedCommitments.RemoteNextCommitInfo
            RemoteCommit = serializedCommitments.RemoteCommit
            RemoteChanges = serializedCommitments.RemoteChanges
            OriginChannels = serializedCommitments.OriginChannels
            LocalParams = serializedCommitments.LocalParams
            LocalNextHTLCId = serializedCommitments.LocalNextHTLCId
            LocalCommit = serializedCommitments.LocalCommit
            LocalChanges = serializedCommitments.LocalChanges
            FundingScriptCoin = serializedCommitments.FundingScriptCoin
            ChannelId = serializedCommitments.ChannelId.DnlChannelId
            ChannelFlags = serializedCommitments.ChannelFlags
        }
        commitments

    override this.WriteJson(writer: JsonWriter, state: Commitments, serializer: JsonSerializer) =
        serializer.Serialize(writer, {
            ChannelId = ChannelId.FromDnl state.ChannelId
            ChannelFlags = state.ChannelFlags
            FundingScriptCoin = state.FundingScriptCoin
            LocalChanges = state.LocalChanges
            LocalCommit = state.LocalCommit
            LocalNextHTLCId = state.LocalNextHTLCId
            LocalParams = state.LocalParams
            OriginChannels = state.OriginChannels
            RemoteChanges = state.RemoteChanges
            RemoteCommit = state.RemoteCommit
            RemoteNextCommitInfo = state.RemoteNextCommitInfo
            RemoteNextHTLCId = state.RemoteNextHTLCId
            RemoteParams = state.RemoteParams
            RemotePerCommitmentSecrets = state.RemotePerCommitmentSecrets
        })

type SerializedChannel = {
    ChannelIndex: int
    Network: Network
    RemoteNodeId: NodeId
    ChanState: ChannelState
    AccountFileName: string
    CounterpartyIP: IPEndPoint
    // this is the amount of confirmations that the counterparty told us that the funding transaction needs
    MinSafeDepth: BlockHeightOffset32
} with
    static member LightningSerializerSettings: JsonSerializerSettings =
        let settings = JsonMarshalling.SerializerSettings
        let commitmentsConverter = CommitmentsJsonConverter()
        settings.Converters.Add commitmentsConverter
        settings

    member this.Commitments: Commitments =
        UnwrapOption
            this.ChanState.Commitments
            "A SerializedChannel is only created once a channel has started \
            being established and must therefore have an initial commitment"

    member this.IsFunder: bool =
        this.Commitments.LocalParams.IsFunder

    member this.Capacity(): Money =
        this.Commitments.FundingScriptCoin.Amount

    member this.Balance(): DotNetLightning.Utils.LNMoney =
        this.Commitments.LocalCommit.Spec.ToLocal

    member this.SpendableBalance(): DotNetLightning.Utils.LNMoney =
        this.Commitments.SpendableBalance()

    // How low the balance can go. A channel must maintain enough balance to
    // cover the channel reserve. The funder must also keep enough in the
    // channel to cover the closing fee.
    member this.MinBalance(): DotNetLightning.Utils.LNMoney =
        this.Balance() - this.SpendableBalance()

    // How high the balance can go. The fundee will only be able to receive up
    // to this amount before the funder no longer has enough funds to cover
    // the channel reserve and closing fee.
    member this.MaxBalance(): DotNetLightning.Utils.LNMoney =
        let capacity = LNMoney.FromMoney <| this.Capacity()
        let channelReserve =
            LNMoney.FromMoney this.Commitments.LocalParams.ChannelReserveSatoshis
        let fee =
            if this.IsFunder then
                let feeRate = this.Commitments.LocalCommit.Spec.FeeRatePerKw
                let weight = COMMITMENT_TX_BASE_WEIGHT
                LNMoney.FromMoney <| feeRate.ToFee weight
            else
                LNMoney.Zero
        capacity - channelReserve - fee

    member this.ChannelId: ChannelId =
        ChannelId.FromDnl this.Commitments.ChannelId

