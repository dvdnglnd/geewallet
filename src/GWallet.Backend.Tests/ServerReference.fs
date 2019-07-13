﻿namespace GWallet.Backend.Tests

open System
open System.Text

open Newtonsoft.Json
open NUnit.Framework

open GWallet.Backend

[<TestFixture>]
type ServerReference() =

    let some_connection_type_irrelevant_for_this_test = { Encrypted = false; Protocol = Http }

    let CreateHistoryInfo(lastSuccessfulCommunication: DateTime) =
        {
            Status = LastSuccessfulCommunication lastSuccessfulCommunication

            //irrelevant for this test
            TimeSpan = TimeSpan.Zero
        } |> Some

    [<Test>]
    member __.``order of servers is kept if non-hostname details are same``() =
        let serverWithHighestPriority =
            {
                HostName = "dlm8yerwlcifs"
                ConnectionType = some_connection_type_irrelevant_for_this_test
                CommunicationHistory = None
            }
        let serverWithLowestPriority =
            {
                 HostName = "eliuh4midkndk"
                 ConnectionType = some_connection_type_irrelevant_for_this_test
                 CommunicationHistory = None
             }
        let serverDetails = ServerRegistry.Serialize [ serverWithHighestPriority; serverWithLowestPriority]

        let serverAPos = serverDetails.IndexOf serverWithHighestPriority.HostName
        let serverBPos = serverDetails.IndexOf serverWithLowestPriority.HostName

        Assert.That(serverAPos, Is.Not.LessThan 0)

        Assert.That(serverBPos, Is.Not.LessThan 0)

        Assert.That(serverBPos, Is.GreaterThan serverAPos, "shouldn't be sorted #1")

        let serverDetailsReverse = ServerRegistry.Serialize [ serverWithLowestPriority; serverWithHighestPriority ]

        let serverAPos = serverDetailsReverse.IndexOf serverWithHighestPriority.HostName
        let serverBPos = serverDetailsReverse.IndexOf serverWithLowestPriority.HostName

        Assert.That(serverAPos, Is.Not.LessThan 0)

        Assert.That(serverBPos, Is.Not.LessThan 0)

        Assert.That(serverAPos, Is.GreaterThan serverBPos, "shouldn't be sorted #2")

    [<Test>]
    member __.``order of servers depends on last successful conn``() =
        let serverWithOldestConnection =
            {
                HostName = "dlm8yerwlcifs"
                ConnectionType = some_connection_type_irrelevant_for_this_test
                CommunicationHistory = CreateHistoryInfo (DateTime.Now - TimeSpan.FromDays 10.0)
            }
        let serverWithMostRecentConnection =
            {
                 HostName = "eliuh4midkndk"
                 ConnectionType = some_connection_type_irrelevant_for_this_test
                 CommunicationHistory = CreateHistoryInfo DateTime.Now
             }
        let serverDetails = ServerRegistry.Serialize [ serverWithOldestConnection; serverWithMostRecentConnection]

        let serverAPos = serverDetails.IndexOf serverWithOldestConnection.HostName
        let serverBPos = serverDetails.IndexOf serverWithMostRecentConnection.HostName

        Assert.That(serverAPos, Is.Not.LessThan 0)

        Assert.That(serverBPos, Is.Not.LessThan 0)

        Assert.That(serverAPos, Is.GreaterThan serverBPos, "should be sorted #1")

        let serverDetailsReverse = ServerRegistry.Serialize [ serverWithMostRecentConnection; serverWithOldestConnection ]

        let serverAPos = serverDetailsReverse.IndexOf serverWithOldestConnection.HostName
        let serverBPos = serverDetailsReverse.IndexOf serverWithMostRecentConnection.HostName

        Assert.That(serverAPos, Is.Not.LessThan 0)

        Assert.That(serverBPos, Is.Not.LessThan 0)

        Assert.That(serverAPos, Is.GreaterThan serverBPos, "should be sorted #2")


        let serverWithNoLastConnection =
            {
                HostName = "dlm8yerwlcifs"
                ConnectionType = some_connection_type_irrelevant_for_this_test
                CommunicationHistory = None
            }

        let serverDetails3 = ServerRegistry.Serialize [ serverWithNoLastConnection; serverWithMostRecentConnection]

        let serverAPos = serverDetails.IndexOf serverWithNoLastConnection.HostName
        let serverBPos = serverDetails.IndexOf serverWithMostRecentConnection.HostName

        Assert.That(serverAPos, Is.Not.LessThan 0)

        Assert.That(serverBPos, Is.Not.LessThan 0)

        Assert.That(serverAPos, Is.GreaterThan serverBPos, "should be sorted #3")

        let serverDetails3Rev = ServerRegistry.Serialize [ serverWithMostRecentConnection; serverWithNoLastConnection]

        let serverAPos = serverDetails3Rev.IndexOf serverWithNoLastConnection.HostName
        let serverBPos = serverDetails3Rev.IndexOf serverWithMostRecentConnection.HostName

        Assert.That(serverAPos, Is.Not.LessThan 0)

        Assert.That(serverBPos, Is.Not.LessThan 0)

        Assert.That(serverAPos, Is.GreaterThan serverBPos, "should be sorted #4")

    [<Test>]
    member __.``stats of server are included in serialization``() =
        let now = DateTime.UtcNow
        let serverWithSomeRecentConnection =
            {
                 HostName = "eliuh4midkndk"
                 ConnectionType = some_connection_type_irrelevant_for_this_test
                 CommunicationHistory = CreateHistoryInfo now
             }
        let serverDetails = ServerRegistry.Serialize [ serverWithSomeRecentConnection ]

        let dayPos = serverDetails.IndexOf (now.Day.ToString())
        Assert.That(dayPos, Is.GreaterThan 0)

        let monthPos = serverDetails.IndexOf (now.Month.ToString())
        Assert.That(monthPos, Is.GreaterThan 0)

        let yearPos = serverDetails.IndexOf (now.Year.ToString())
        Assert.That(yearPos, Is.GreaterThan 0)

        let hourPos = serverDetails.IndexOf (now.Hour.ToString())
        Assert.That(hourPos, Is.GreaterThan 0)

        let minPos = serverDetails.IndexOf (now.Minute.ToString())
        Assert.That(minPos, Is.GreaterThan 0)

    [<Test>]
    member __.``serialization is JSON based (for readability and consistency with rest of wallet)``() =
        let now = DateTime.UtcNow
        let serverWithSomeRecentConnection =
            {
                 HostName = "eliuh4midkndk"
                 ConnectionType = some_connection_type_irrelevant_for_this_test
                 CommunicationHistory = CreateHistoryInfo DateTime.UtcNow
             }
        let serverDetails = ServerRegistry.Serialize [ serverWithSomeRecentConnection ]

        let deserializedServerDetails = JsonConvert.DeserializeObject serverDetails
        Assert.That(deserializedServerDetails, Is.Not.Null)

    [<Test>]
    member __.``details of server are included in serialization``() =
        let port = 50001u
        let serverWithSomeRecentConnection =
            {
                 HostName = "eliuh4midkndk"
                 ConnectionType = { Encrypted = false; Protocol = Tcp port }
                 CommunicationHistory = None
             }
        let serverDetails = ServerRegistry.Serialize [ serverWithSomeRecentConnection ]

        let portPos = serverDetails.IndexOf (port.ToString())
        Assert.That(portPos, Is.GreaterThan 0)

    [<Test>]
    member __.``serializing and deserializing leads to same result (no order regarded in this test)``() =
        let tcpServerHostName = "tcp"
        let tcpServerWithNoHistory =
            {
                 HostName = tcpServerHostName
                 ConnectionType = { Encrypted = false; Protocol = Tcp 50001u }
                 CommunicationHistory = None
             }

        let timeSpanForHttpServer = TimeSpan.FromSeconds 1.0
        let httpServerHostName = "http"
        let lastSuccessfulCommunication = DateTime.UtcNow
        let httpSuccessfulServer =
            {
                 HostName = httpServerHostName
                 ConnectionType = { Encrypted = false; Protocol = Http }
                 CommunicationHistory = Some({
                                                Status = LastSuccessfulCommunication lastSuccessfulCommunication
                                                TimeSpan = timeSpanForHttpServer
                                             })
             }

        let httpsServerHostName1 = "https1"
        let timeSpanForHttpsServer = TimeSpan.FromSeconds 2.0
        let exInfo = { TypeFullName = "SomeNamespace.SomeException" ; Message = "argh" }
        let httpsFailureServer1 =
            {
                 HostName = httpsServerHostName1
                 ConnectionType = { Encrypted = true; Protocol = Http }
                 CommunicationHistory = Some({
                                                Status = Fault (exInfo, None)
                                                TimeSpan = timeSpanForHttpsServer
                                             })
             }
        let httpsServerHostName2 = "https2"
        let httpsFailureServer2 =
            {
                 HostName = httpsServerHostName2
                 ConnectionType = { Encrypted = true; Protocol = Http }
                 CommunicationHistory = Some({
                                                Status = Fault (exInfo, Some lastSuccessfulCommunication)
                                                TimeSpan = timeSpanForHttpsServer
                                             })
             }

        let serverDetails = ServerRegistry.Serialize [ tcpServerWithNoHistory
                                                       httpSuccessfulServer
                                                       httpsFailureServer1
                                                       httpsFailureServer2 ]

        let deserializedServers = ServerRegistry.Deserialize serverDetails |> List.ofSeq
        Assert.That(deserializedServers.Length, Is.EqualTo 4)

        let tcpServers = Seq.filter (fun server -> server.HostName = tcpServerHostName)
                                    deserializedServers
                                        |> List.ofSeq
        Assert.That(tcpServers.Length, Is.EqualTo 1)
        let tcpServer = tcpServers.[0]
        Assert.That(tcpServer.HostName, Is.EqualTo tcpServerHostName)
        Assert.That(tcpServer.ConnectionType.Encrypted, Is.EqualTo false)
        Assert.That(tcpServer.CommunicationHistory, Is.EqualTo None)

        let httpServers = Seq.filter (fun server -> server.HostName = httpServerHostName)
                                      deserializedServers
                                          |> List.ofSeq
        Assert.That(httpServers.Length, Is.EqualTo 1)
        let httpServer = httpServers.[0]
        Assert.That(httpServer.HostName, Is.EqualTo httpServerHostName)
        Assert.That(httpServer.ConnectionType.Encrypted, Is.EqualTo false)
        match httpServer.CommunicationHistory with
        | None -> Assert.Fail "http server should have some historyinfo"
        | Some historyInfo ->
            Assert.That(historyInfo.TimeSpan, Is.EqualTo timeSpanForHttpServer)
            match historyInfo.Status with
            | Fault _ ->
                Assert.Fail "http server should be successful, not failure"
            | LastSuccessfulCommunication lsc ->
                Assert.That(lsc, Is.EqualTo lastSuccessfulCommunication)

        let https1Servers = Seq.filter (fun server -> server.HostName = httpsServerHostName1)
                                        deserializedServers
                                          |> List.ofSeq
        Assert.That(https1Servers.Length, Is.EqualTo 1)
        let httpsServer1 = https1Servers.[0]
        Assert.That(httpsServer1.HostName, Is.EqualTo httpsServerHostName1)
        Assert.That(httpsServer1.ConnectionType.Encrypted, Is.EqualTo true)
        match httpsServer1.CommunicationHistory with
        | None -> Assert.Fail "https server should have some historyinfo"
        | Some historyInfo ->
            Assert.That(historyInfo.TimeSpan, Is.EqualTo timeSpanForHttpsServer)
            match historyInfo.Status with
            | Fault (fault, maybeLsc) ->
                Assert.That(fault.TypeFullName, Is.EqualTo exInfo.TypeFullName)
                Assert.That(fault.Message, Is.EqualTo exInfo.Message)
                Assert.That(maybeLsc, Is.EqualTo None)
            | _ ->
                Assert.Fail "https server should be fault, not successful"

        let https2Servers = Seq.filter (fun server -> server.HostName = httpsServerHostName2)
                                        deserializedServers
                                          |> List.ofSeq
        Assert.That(https2Servers.Length, Is.EqualTo 1)
        let httpsServer2 = https2Servers.[0]
        Assert.That(httpsServer2.HostName, Is.EqualTo httpsServerHostName2)
        Assert.That(httpsServer2.ConnectionType.Encrypted, Is.EqualTo true)
        match httpsServer2.CommunicationHistory with
        | None -> Assert.Fail "https server should have some historyinfo"
        | Some historyInfo ->
            Assert.That(historyInfo.TimeSpan, Is.EqualTo timeSpanForHttpsServer)
            match historyInfo.Status with
            | Fault (fault, maybeLsc) ->
                Assert.That(fault.TypeFullName, Is.EqualTo exInfo.TypeFullName)
                Assert.That(fault.Message, Is.EqualTo exInfo.Message)
                Assert.That(maybeLsc, Is.EqualTo (Some lastSuccessfulCommunication))
            | _ ->
                Assert.Fail "https server should be fault, not successful"
