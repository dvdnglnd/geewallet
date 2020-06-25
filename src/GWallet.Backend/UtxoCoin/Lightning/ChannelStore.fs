namespace GWallet.Backend.UtxoCoin.Lightning

open System.IO

open DotNetLightning.Channel
open DotNetLightning.Utils

open GWallet.Backend
open GWallet.Backend.UtxoCoin
open GWallet.Backend.FSharpUtil.UwpHacks
open GWallet.Backend.UtxoCoin.Lightning.Primitives

type FundingBroadcastButNotLockedData = {
    TxId: TxIdWrapper
    MinimumDepth: uint32
} with
    member this.GetRemainingConfirmations(): Async<uint32> =
        async {
            let! confirmationCount =
                UtxoCoin.Server.Query
                    Currency.BTC
                    (UtxoCoin.QuerySettings.Default ServerSelectionMode.Fast)
                    (UtxoCoin.ElectrumClient.GetConfirmations (this.TxId.ToString()))
                    None
            if confirmationCount < this.MinimumDepth then
                let remainingConfirmations = this.MinimumDepth - confirmationCount
                return remainingConfirmations
            else
                return 0u
        }

type ChannelStatus =
    | FundingBroadcastButNotLocked of FundingBroadcastButNotLockedData
    | Active
    | Broken

type ChannelInfo = {
    ChannelId: ChannelIdWrapper
    IsFunder: bool
    Balance: decimal
    SpendableBalance: decimal
    Capacity: decimal
    MaxBalance: decimal
    MinBalance: decimal
    Status: ChannelStatus
    Currency: Currency
} with
    static member internal FromSerializedChannel (serializedChannel: SerializedChannel)
                                                     : ChannelInfo = {
        ChannelId = serializedChannel.ChannelId
        IsFunder = serializedChannel.IsFunder
        Balance = serializedChannel.Balance().BTC |> decimal
        SpendableBalance = serializedChannel.SpendableBalance().BTC |> decimal
        Capacity = serializedChannel.Capacity().ToUnit(NBitcoin.MoneyUnit.BTC)
        MaxBalance = serializedChannel.MaxBalance().BTC |> decimal
        MinBalance = serializedChannel.MinBalance().BTC |> decimal
        Currency = Currency.BTC
        Status =
            match serializedChannel.ChanState with
            | ChannelState.Normal _ -> ChannelStatus.Active
            | ChannelState.WaitForFundingConfirmed waitForFundingConfirmedData ->
                let txId = TxIdWrapper.FromHash waitForFundingConfirmedData.Commitments.FundingScriptCoin.Outpoint.Hash
                let minimumDepth = serializedChannel.MinSafeDepth.Value
                let fundingBroadcastButNotLockedData = {
                    TxId = txId
                    MinimumDepth = minimumDepth
                }
                ChannelStatus.FundingBroadcastButNotLocked fundingBroadcastButNotLockedData
            | _ -> ChannelStatus.Broken
    }



type ChannelStore(account: NormalUtxoAccount) =
    static member ChannelFilePrefix = "chan-"
    static member ChannelFileEnding = ".json"

    member this.Account = account
    member this.Currency = (this.Account :> IAccount).Currency

    member this.AccountDir: DirectoryInfo =
        Config.GetConfigDir this.Currency AccountKind.Normal

    member this.ChannelDir: DirectoryInfo =
        let subdirectory = SPrintF1 "%s-lightning" (this.Account :> IAccount).PublicAddress
        Path.Combine (this.AccountDir.FullName, subdirectory) |> DirectoryInfo

    member this.ListChannelIds(): seq<ChannelIdWrapper> =
        let extractChannelId path: Option<ChannelIdWrapper> =
            let fileName = Path.GetFileName path
            let withoutPrefix = fileName.Substring ChannelStore.ChannelFilePrefix.Length
            let withoutEnding =
                withoutPrefix.Substring(
                    0,
                    withoutPrefix.Length - ChannelStore.ChannelFileEnding.Length
                )
            ChannelIdWrapper.Parse withoutEnding

        if this.ChannelDir.Exists then
            let files =
                Directory.GetFiles(this.ChannelDir.ToString())
            files |> Seq.choose extractChannelId
        else
            Seq.empty

    member this.ChannelFileName (channelId: ChannelIdWrapper): string =
        Path.Combine(
            this.ChannelDir.FullName,
            SPrintF3
                "%s%s%s"
                ChannelStore.ChannelFilePrefix
                (channelId.ToString())
                ChannelStore.ChannelFileEnding
        )

    member internal this.LoadChannel (channelId: ChannelIdWrapper): SerializedChannel =
        let fileName = this.ChannelFileName channelId
        let json = File.ReadAllText fileName
        Marshalling.DeserializeCustom<SerializedChannel> (
            json,
            SerializedChannel.LightningSerializerSettings
        )

    member internal this.SaveChannel (serializedChannel: SerializedChannel) =
        let fileName = this.ChannelFileName serializedChannel.ChannelId
        let json =
            Marshalling.SerializeCustom(
                serializedChannel,
                SerializedChannel.LightningSerializerSettings
            )
        if not this.ChannelDir.Exists then
            this.ChannelDir.Create()
        File.WriteAllText(fileName, json)

    member this.ChannelInfo (channelId: ChannelIdWrapper): ChannelInfo =
        let serializedChannel = this.LoadChannel channelId
        ChannelInfo.FromSerializedChannel serializedChannel

    member this.ListChannelInfos(): seq<ChannelInfo> = seq {
        for channelId in this.ListChannelIds() do
            yield this.ChannelInfo channelId
    }

