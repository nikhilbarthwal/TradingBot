namespace Trading

open System
open Websocket.Client


type SocketAdapter =
    abstract member Url: string with get
    abstract member Timeout: int
    abstract member Receiver: string -> unit
    abstract member Reset: ReconnectionType -> unit


type SocketConnection(adapter: SocketAdapter) =

    let url: string = adapter.Url
    let receiver (message: ResponseMessage): unit = adapter.Receiver message.Text
    let reset (info: Models.ReconnectionInfo): unit = adapter.Reset info.Type

    let client, reconnect, receive, task =
        try
            let cl = new WebsocketClient(Uri(url))
            cl.ReconnectTimeout <- TimeSpan.FromSeconds(adapter.Timeout)
            let rc = ObservableExtensions.Subscribe(cl.ReconnectionHappened, reset)
            let rv = ObservableExtensions.Subscribe(cl.MessageReceived, receiver)
            (cl, rc, rv, cl.Start())
        with e -> Log.Error("Socket", $"Exception in {url} socket connection -> {e}")

    member this.Send (msg:string) = client.Send msg

    interface IDisposable with
        member this.Dispose() =
            receive.Dispose() ; reconnect.Dispose() ; client.Dispose()
            Utils.Wait(adapter.Timeout)
            if task.IsCompleted then
                Log.Info("Socket", $"Closed Socket connection {url}")
            else
                Log.Warning("Socket", $"Unable to close socket connection {url}")
