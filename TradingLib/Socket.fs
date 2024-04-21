namespace CryptoBot

open Config
open System
open Websocket.Client

type Socket(Url: string, ticker: ticker, Receiver: string -> unit) =

    let receiver (message: ResponseMessage): unit = Receiver message.Text
    let reset (info: Models.ReconnectionInfo): unit =
        Log.Warning(0, $"Resetting stream for {ticker} -> {info.Type}")

    let client, reconnect, receive, task =
        try
            let cl = new WebsocketClient(Uri(Url))
            cl.ReconnectTimeout <- TimeSpan.FromSeconds(15.0)
            let rc = ObservableExtensions.Subscribe(cl.ReconnectionHappened, reset)
            let rv = ObservableExtensions.Subscribe(cl.MessageReceived, receiver)
            (cl, rc, rv, cl.Start())
        with e -> Log.Error(0, $"Exception in {Url} socket connection -> {e}")

    member this.Send (msg:string) = client.Send msg

    interface IDisposable with
        member this.Dispose() =
            receive.Dispose() ; reconnect.Dispose() ; client.Dispose()
            Utils.Wait(SocketCloseTimeout)
            if task.IsCompleted then
                Log.Info(0, $"Closed Socket connection {Url}")
            else
                Log.Warning(0, $"Unable to close socket connection {Url}")


module Socket =

    type struct Adapter =
        menber this.Timeout: int
        member this.Recieve(string -> unit)
        member this.Reconnection(string -> unit)
        member this.Send(msg: string)
        member this.Url

    type private Connection(Adapter)

    module Adapter =
        type Single:
            member this.Url: string
            member this.Start(Ticket list -> string list)

        type Multi:
            member this.Url: ticker -> string
            member this.Start(Ticker -> string list)



    module Connection:
        type Single(Adapter.Single): Data.Source
        type Multi(Adapter.Multi): Data.Source
