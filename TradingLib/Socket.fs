namespace TradingLib

open Websocket.Client


module Socket =

    type private parameters = {|
        Url: string
        Timeout: int
        Receive: string -> unit
        Reconnection: string -> unit
        Send: string -> unit
        Tag: string |}

    type private Socket (p: parameters) =

        let client, reconnect, receive, task =
            try
                let cl = new WebsocketClient(System.Uri(p.Url))
                cl.ReconnectTimeout <- System.TimeSpan.FromSeconds(p.Timeout)
                let rc = System.ObservableExtensions.Subscribe(
                    cl.ReconnectionHappened,
                    fun info -> p.Reconnection(info.Type.ToString()))
                let rv = System.ObservableExtensions.Subscribe(
                    cl.MessageReceived, fun msg -> p.Receive(msg.Text))
                (cl, rc, rv, cl.Start())
            with ex ->
                Log.Exception(p.Tag, $"Exception in {p.Url} socket connection") ex

        member this.Send (msg:string) = client.Send msg

        interface System.IDisposable with
            member this.Dispose() =
                receive.Dispose() ; reconnect.Dispose() ; client.Dispose()
                Utils.Wait(p.Timeout)
                if task.IsCompleted then
                    Log.Info(p.Tag, $"Closed Socket connection for {p.Url}")
                else
                    Log.Warning(p.Tag,
                                $"Unable to close socket connection for {p.Url}")

    (* module Adapter =

        Common:
- Tickers list
- Timeout
- Start(ticker -> unit)
- Dispose()
- Buffer

singularAdapter
- Url: string
- Receiver(string, ticker -> bar -> unit)
- Reconnection(string -> unit)
- Send(msg -> ())
- Initialze( unit -> unit)


multiAdapter
- Url: string -> string
- Receiver(ticker, string, bar -> unit)
- Reconnection(ticker -> string -> unit)
- Send(ticker -> msg -> ())

        type Single =
        type Multi =


    module Connection =

        let getExchange(Adapter.Base): Exchanage

        type Singe(z: Adapter.Singular) =
            let exchange = getExchange(z)
            let connections = ...

            interface Source
        type Multi(z: Adapter.Singular) =

            let exchange = getExchange(z)
            let adapter ticker: socketParams =
            let connections = Utils.CreateDictionary(z.Tickets, adapter >> Socket)

            interface Source

        let Singular(z: Adapter.Singular): Source =
        let Multi(z: Adapter.Singular): Source =



*)
