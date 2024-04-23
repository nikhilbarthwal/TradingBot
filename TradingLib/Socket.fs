namespace TradingLib

open Websocket.Client


module Socket =

    type private parameters = {|
        Url: string
        Timeout: int
        Receive: string -> unit
        Reconnect: string -> unit
        Send: string -> unit
        Tag: string |}

    type private Socket (p: parameters) =

        let client, reconnect, receive, task =
            try
                let cl = new WebsocketClient(System.Uri(p.Url))
                cl.ReconnectTimeout <- System.TimeSpan.FromSeconds(p.Timeout)
                let rc = System.ObservableExtensions.Subscribe(
                    cl.ReconnectionHappened,
                    fun info -> p.Reconnect(info.Type.ToString()))
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

    module Adapter =

        type Common =
            inherit System.IDisposable
            abstract Tickers: Ticker list
            abstract Timeout: int
            abstract Start: Ticker -> unit
            abstract Buffer: Buffer
            abstract Size: int

        type Single =
            inherit Common
            abstract Url: string
            abstract Receive: string * (Ticker -> Bar -> unit) -> unit
            abstract Reconnect: string -> unit
            abstract Send: string -> unit
            abstract Initialize: unit -> unit
            abstract Tag: string

        type Multi =
            inherit Common
            abstract Url: Ticker -> string
            abstract Receive: Ticker * string * (Bar -> unit) -> unit
            abstract Reconnect: Ticker * string -> unit
            abstract Send: Ticker * string -> unit
            abstract Tag: Ticker -> string

    module Source =

        type private SingleSource(z: Adapter.Single) =
            let exchange = Data.Exchange(z.Tickers, z.Size, z.Buffer)
            let insert ticker = exchange[ticker].Insert
            let connection: System.IDisposable = new Socket {|
                Url = z.Url
                Timeout = z.Timeout
                Receive = fun msg -> z.Receive(msg, insert)
                Reconnect = z.Reconnect
                Send = z.Send
                Tag = z.Tag |}

            interface Data.Source with
                member this.Data = exchange.Data
                member this.Tickers = z.Tickers
                member this.Dispose() = z.Dispose() ; connection.Dispose()

        type private MultiSource(z: Adapter.Multi) =

            let exchange = Data.Exchange(z.Tickers, z.Size, z.Buffer)
            let adapter ticker: parameters ={|
                Url = z.Url ticker
                Timeout = z.Timeout
                Receive = fun msg -> z.Receive(ticker, msg, exchange[ticker].Insert)
                Reconnect = fun msg -> z.Reconnect(ticker, msg)
                Send = fun msg -> z.Send(ticker, msg)
                Tag = z.Tag ticker |}
            let socket ticker = new Socket(adapter ticker) :> System.IDisposable
            let connections = Utils.CreateDictionary(z.Tickers, socket)

            interface Data.Source with
                member this.Data = exchange.Data
                member this.Tickers = z.Tickers
                member this.Dispose() =
                    z.Dispose()
                    for connection in connections.Values do connection.Dispose()

        let Single(z: Adapter.Single): Data.Source = new SingleSource(z)
        let Multi(z: Adapter.Multi): Data.Source = new MultiSource(z)
