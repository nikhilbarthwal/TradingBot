namespace TradingLib


type Buffer = abstract member Ingest: Bar * (Bar -> unit) -> bool

module Buffer =

    type private Bucket() =
        let mutable data = Bar()
        let mutable count = 0
        let merge (b: Bar) =
            let pF = float count
            let tF = float <| count + 1
            let pL = int64 count
            let tL = int64 <| count + 1
            count <- count + 1
            let avgFloat (p, c) = (pF * p + c) / tF
            let avgLong (p, c) = (pL * p + c) / tL
            Bar <| {| Open   = avgFloat(data.Open, b.Open)
                      Close  = avgFloat(data.Close, b.Close)
                      High   = max data.High b.High
                      Low    = min data.Low b.Low
                      Time   = avgLong(data.Epoch, b.Epoch)
                      Volume = avgLong(data.Volume, b.Volume) |}

        member this.Data = assert data.Valid ; data
        // member this.Add(x: Bar) = if data.Valid then data <- merge x else data <- x
        member this.Add(x: Bar) =
            if data.Valid then
                let t = data
                data <- merge x
                Log.Debug("Merge", $"{x}: {t} -> {data}")
            else
                Log.Debug("Merge", $"{x}: Initial")
                data <- x

        member this.Reset() = data <-Bar()
        member this.Count = count

    type private Buckets(size: int) =
        let mutable pos = 0
        let buckets = [| for _ in [1 .. size] do Bucket() |]
        let index k = (pos + k) % size

        member this.Item with get(k: int) = buckets[index(k)]
        member this.Previous() = buckets[0].Data.Epoch
        member this.Shift(k) = pos <- (pos + k) % size
        member this.Reset() =
            pos <- 0 ; for i in [0 .. size - 1] do buckets[i].Reset()

    type private LinearBuffer(interval: time, size: int) =

        let buckets = Buckets(size)
        let mutable previous: time = 0L
        let floor (t:time) = t - (t % interval)

        let extrapolate (diff: int) (output: Bar -> unit) (i: int): unit =
            let t = previous + interval * (int64 i)
            let p, c = buckets[0].Data, buckets[diff].Data
            let dt = c.Epoch - p.Epoch

            let extrapolateF (prev: float, curr: float): float =
                prev + ((curr - prev) * (float <| t - p.Epoch)) / (float <| dt)

            let extrapolateL (prev: int64, curr: int64): int64 =
                prev + ((curr - prev) * (t - p.Epoch)) / dt

            output <| Bar({| Open   = extrapolateF(p.Open, c.Open)
                             Close  = extrapolateF(p.Close, c.Close)
                             High   = extrapolateF(p.High, c.High)
                             Low    = extrapolateF(p.Low, c.Low)
                             Time   = extrapolateL(p.Epoch, c.Epoch)
                             Volume = extrapolateL(p.Volume, c.Volume) |})

        interface Buffer with
            member this.Ingest(input: Bar, output: Bar -> unit): bool =
                let current = floor input.Epoch
                if buckets[0].Count = 0 then
                    buckets[0].Add input
                    if input.Epoch % interval = 0 then output(input)
                    previous <- current
                    true
                else
                    let temp = buckets.Previous()
                    if input.Epoch < temp then
                        Log.Debug("Error", $"Current = {input.Epoch} / Previous = {temp} / Floor = {current}")  // input.ToString())
                    else
                        Log.Debug("Ingest", $"{input.Epoch} / Previous = {temp} / Floor = {current}")  // input.ToString())
                    output <| input
                    buckets[0].Add input
                    true

                (*let current = floor input.Epoch
                if buckets[0].Count = 0 then
                    buckets[0].Add input
                    if input.Epoch % interval = 0 then output(input)
                    previous <- current
                    true
                else
                    if input.Epoch < buckets.Previous() then false else
                        let diff = int <| (current - previous) / interval
                        if diff >= size then
                            buckets.Reset() ; false
                        else
                            buckets[diff].Add input
                            for i in [1 .. diff] do (extrapolate diff output i)
                            buckets.Shift(diff) ; true
                *)

    let Linear z: Buffer = LinearBuffer z :> Buffer
