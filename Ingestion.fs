namespace CryptoBot


type Ingestion = member Ingest: Bar -> ()

module Ingestion =

    type Response = Prices of tick list | Error of string

    type Scheme =
        abstract member Initial: tick -> Response
        abstract member Update: tick -> tick -> Response

    let private floor (t:time) = t + BufferInterval - (t % BufferInterval)

    type private LinearScheme(ticker: ticker) =

        let prices (current:tick) (previous: tick): tick list =
            let dTime  = current.Time  - previous.Time
            let dPrice = current.Price - previous.Price
            assert (dTime > 0)

            let check (timestamp: time) = current.Time >= timestamp
            let next  (timestamp: time) = BufferInterval + timestamp

            let linearize (timestamp: time) =
                let dy, dx = float (timestamp - previous.Time) , float dTime
                let price = previous.Price + (dPrice * dy) / dx
                { Price = Normalize price ; Time = timestamp }

            Collect(floor previous.Time, check, next) linearize

        interface Scheme with

            member this.Initial (current: tick): Response =
                let modulus = current.Time % BufferInterval
                Prices(if modulus = 0 then [current] else [])

            member this.Update (current: tick) (previous:tick): Response =
                match (current.Time - previous.Time) with
                | 0L -> Prices([])
                | d when d < ResetThresholdTime -> Prices(prices current previous)
                | _ -> Error("Stream Threshold exceeded for Linear scheme")

    type private Buckets(size:int) =

        let count = NewArray size 0.0
        let total = NewArray size 0.0

        member this.Average index: float option =
            if count[index] = 0 then None
                else Some(total[index] / count[index])

        member this.Add index price: unit =
            total[index] <- total[index] + price
            count[index] <- count[index] + 1.0

        member this.Reset index: unit =
            total[index] <- 0.0
            count[index] <- 0.0

        member this.Copy(indexTo, indexFrom): unit =
            total[indexTo] <- total[indexFrom]
            count[indexTo] <- count[indexFrom]


    type private BucketScheme(ticker: ticker) =

        let buckets = Buckets(BucketSchemeBufferSize)

        let getInterval (diff:time): int option =
            [0 .. BucketSchemeBufferSize - 1]
            |> List.tryFind (fun n -> (int64 n) * BufferInterval = diff)

        let adjust (interval:int) (position:int) =
            let index = position + interval
            if index >= BucketSchemeBufferSize then buckets.Reset(position)
                else buckets.Copy(position, index)

        let prices (interval: int) (price: float) (baseTime:time) : tick list =
            buckets.Add interval <| price
            if interval = 0 then [] else
                let average: float = (buckets.Average 0).Value
                [0 .. BucketSchemeBufferSize-1] |> List.iter (adjust interval)
                let delta = (price - average) / (float interval)
                let mapping (n:int) =
                        { Price = Normalize <| average + (float (n - 1)) * delta
                          Time  = baseTime + (int64 n) * BufferInterval}
                List.map mapping [1 .. interval]

        interface Scheme with
            member this.Initial (current: tick): Response =
                buckets.Add 0 <| current.Price ; Prices([])

            member this.Update (current: tick) (previous:tick): Response =
                let previousBase: time = floor previous.Time
                let currentBase: time = floor current.Time
                match (getInterval <| currentBase - previousBase) with
                | Some(index) -> Prices(prices index current.Price previousBase)
                | None        -> Error("Hop exceeded for Bucket scheme")


    let Linear (params: {| interval: float |}) (store: ticker -> unit): Ingestion =
    let Bucket (params: {| size: int |}) (store: ticker -> unit): Ingestion =
