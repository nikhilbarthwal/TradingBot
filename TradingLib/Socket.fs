namespace TradingLib

open Websocket.Client


type Socket (p: {|
        Url: string
        Timeout: int
        Receive: string -> unit
        Reconnection: string -> unit
        Send: string -> unit
        Tag: string |}) =

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
                Log.Warning(p.Tag, $"Unable to close socket connection for {p.Url}")

(*
module Socket =

	type private Connection() =

	module Adapter =

		type Singular =
		type Multi =
		
	module Source =
	
		let Singular(z: Adapter.Singular): Source =
		let Multi(z: Adapter.Singular): Source =
*)

