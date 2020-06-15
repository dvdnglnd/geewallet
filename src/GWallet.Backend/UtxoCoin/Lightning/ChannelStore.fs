namespace GWallet.Backend.UtxoCoin.Lightning

open System.IO

open GWallet.Backend
open GWallet.Backend.UtxoCoin
open GWallet.Backend.FSharpUtil.UwpHacks

(*
type ChannelStatus =
    | Active of ActiveChannelSummary
    | Pending of PendingChannelSummary
    | Funded of FundedChannelSummary
    | Broken of BrokenChannelSummary
*)

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

    member this.ListChannelIds(): seq<ChannelId> =
        let extractChannelId path: Option<ChannelId> =
            let fileName = Path.GetFileName path
            let withoutPrefix = fileName.Substring ChannelStore.ChannelFilePrefix.Length
            let withoutEnding =
                withoutPrefix.Substring(
                    0,
                    withoutPrefix.Length - ChannelStore.ChannelFileEnding.Length
                )
            ChannelId.Parse withoutEnding

        if this.ChannelDir.Exists then
            let files =
                Directory.GetFiles(this.ChannelDir.ToString())
            files |> Seq.choose extractChannelId
        else
            Seq.empty

    member this.ChannelFileName (channelId: ChannelId): string =
        Path.Combine(
            this.ChannelDir.FullName,
            SPrintF3
                "%s%s%s"
                ChannelStore.ChannelFilePrefix
                (channelId.ToString())
                ChannelStore.ChannelFileEnding
        )

    member this.LoadChannel (channelId: ChannelId): SerializedChannel =
        let fileName = this.ChannelFileName channelId
        let json = File.ReadAllText fileName
        Marshalling.DeserializeCustom<SerializedChannel> (
            json,
            SerializedChannel.LightningSerializerSettings
        )

    member this.SaveChannel (serializedChannel: SerializedChannel) = 
        let fileName = this.ChannelFileName serializedChannel.ChannelId
        let json =
            Marshalling.SerializeCustom(
                serializedChannel,
                SerializedChannel.LightningSerializerSettings
            )
        if not this.ChannelDir.Exists then
            this.ChannelDir.Create()
        File.WriteAllText(fileName, json)

