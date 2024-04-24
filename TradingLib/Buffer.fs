namespace TradingLib

type Buffer = abstract member Insert: Bar * (Bar -> unit) -> bool

module Buffer =

    let private floor (t:time) (interval: time) = t - (t % interval)

    let private merge (b1: Bar, b2: Bar): Bar =
        Bar <| {| Open   = (b1.Open + b2.Open) / 2.0
                  Close  = (b1.Close + b2.Close) / 2.0
                  High   = max b1.High b2.High
                  Low    = min b1.Low b2.Low
                  Time   = (b1.Epoch + b2.Epoch) / 2L
                  Volume = (b1.Volume + b2.Volume) / 2L |}

    type private LinearBuffer(interval: time, gap: int) =

        let mutable previous: Maybe<Bar> = No

        let extrapolate (start: time, stop: time) (p: Bar, c: Bar)
                        (output: Bar -> unit) (t: time): unit =

            let dt = stop - start

            let extrapolateF (prev: float, curr: float): float =
                let dx = (curr - prev)
                prev + (dx * (float <| t - start)) / (float <| dt)

            let extrapolateL (prev: int64, curr: int64): int64 =
                let dx = (curr - prev)
                prev + (dx * (t - start)) / dt

            output <| Bar({| Open   = extrapolateF(p.Open, c.Open)
                             Close  = extrapolateF(p.Close, c.Close)
                             High   = extrapolateF(p.High, c.High)
                             Low    = extrapolateF(p.Low, c.Low)
                             Time   = extrapolateL(p.Epoch, c.Epoch)
                             Volume = extrapolateL(p.Volume, c.Volume) |})

        let update (prev: Bar) (curr: Bar) (output: Bar -> unit): bool =
            let start: time = floor prev.Epoch interval
            let diff: int = int <| ((floor curr.Epoch interval) - start) / interval

            if diff >= gap then false else
                if diff = 0 then
                    previous <- Yes(merge(prev, curr)) ; true
                else
                    let insert = extrapolate (start, curr.Epoch) (prev, curr) output
                    for i in [1 .. diff] do (insert <| start + (int64 i) * interval)
                    previous <- Yes(curr) ; true

        interface Buffer with member this.Insert(input, output): bool =
                                     match previous with
                                     | Yes(prev) -> update prev input output
                                     | No -> previous <- Yes(input) ; true

(*

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

    type private BucketBuffer(interval: time, count: int) =

        let buckets = Buckets(count)

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


    let Linear (params: {| interval: float |}) (store: ticker -> unit): Preprocess =
    let Bucket (params: {| size: int |}) (store: ticker -> unit): Preprocess =
*)
    let Linear z: Buffer = LinearBuffer z :> Buffer
