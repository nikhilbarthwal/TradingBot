namespace TradingLib

open Websocket.Client


module Socket =

    type Adapter =
        abstract Url: string
        abstract Timeout: int
        abstract Receive: string -> unit
        abstract Reconnection: string -> unit
        abstract Send: string -> unit

    type Connection(adapter: Adapter, tag: string) =
        let url = adapter.Url
        let client, reconnect, receive, task =
            try
                let cl = new WebsocketClient(System.Uri(adapter.Url))
                cl.ReconnectTimeout <- System.TimeSpan.FromSeconds(adapter.Timeout)
                let rc = System.ObservableExtensions.Subscribe(
                    cl.ReconnectionHappened,
                    fun info -> adapter.Reconnection(info.Type.ToString()))
                let rv = System.ObservableExtensions.Subscribe(
                    cl.MessageReceived, fun msg -> adapter.Receive(msg.Text))
                (cl, rc, rv, cl.Start())
            with ex ->
                Log.Error(tag, ex, $"Exception in {url} socket connection -> {ex}")

        member this.Send (msg:string) = client.Send msg

        interface System.IDisposable with
            member this.Dispose() =
                receive.Dispose() ; reconnect.Dispose() ; client.Dispose()
                Utils.Wait(adapter.Timeout)
                if task.IsCompleted then
                    Log.Info(tag, $"Closed Socket connection for {url}")
                else
                    Log.Warning(tag, $"Unable to close socket connection for {url}")
