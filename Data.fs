namespace CryptoBot

open Config
open System

type Buffer(ticker: ticker) =

    let mutable last: tick option = None
    let data: tick[] = Utils.NewArray BufferSize <| { Price = -1.0 ; Time = 0 }
    let mutable pos: int = 0
    let mutable ready = false
    let mutable count: int = 0
    let ingestion: Ingestion.Scheme = Ingestion.Get ticker
    let Lock = Object()
    let Ready(): bool = lock Lock (fun () -> ready)

    let add (ticks: tick list): unit =
        lock Lock (fun () ->
            for tick in ticks do
                Log.Info(0, $"Price of {ticker} = {tick}")
                data[pos] <- tick
                pos <- pos + 1 ; if pos = BufferSize then pos <- 0
                count <- count + 1 ; if count = BufferSize then ready <- true)

    let reset (message: string): unit =
        Log.Warning(0, $"Resetting stream for {ticker}, {message}")
        lock Lock (fun () -> pos <- 0 ; count <- 0 ; last <- None ; ready <- false)

    let get(buffer: float[]): bool =
        let previous: time = data[(BufferSize + pos - 1) % BufferSize].Time
        let diff: time = (Utils.CurrentTime() - previous) - ResetThresholdTime
        if diff > 0 then
            reset($"Stream for {ticker} exceeded threshold by {diff}") ; false
        else
            lock Lock (fun () ->
                for index in [1 .. buffer.Length] do
                    let n = (BufferSize + pos - index) % BufferSize
                    buffer[index - 1] <- data[n].Price)
            true

    member this.Get(buffer: float[]): bool =
        let status = Ready() in if status then (get buffer) else false

    member this.Update(tick: tick option): unit =
        match tick with
        | None -> ()
        | Some(current) ->
            let response = match last with
                           | Some(previous) -> ingestion.Update  current previous
                           | None ->           ingestion.Initial current

            match response with
            | Ingestion.Error(message) -> reset(message)
            | Ingestion.Prices(ticks) -> last <- Some(current) ; add ticks
