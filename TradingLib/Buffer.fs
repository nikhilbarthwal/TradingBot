namespace TradingLib
#if DEBUG
open System.Diagnostics
#endif


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
            let avgFloat (p, c) = (pF * p + c) / tF
            let avgLong (p, c) = (pL * p + c) / tL
            Bar <| {| Open   = avgFloat(data.Open, b.Open)
                      Close  = avgFloat(data.Close, b.Close)
                      High   = max data.High b.High
                      Low    = min data.Low b.Low
                      Time   = avgLong(data.Epoch, b.Epoch)
                      Volume = avgLong(data.Volume, b.Volume) |}

        member this.Data = Debug.Assert(count > 0, $"count = {count}") ; assert (count > 0) ; data
        member this.Reset() = data <-Bar() ; count <- 0
        member this.Count = count
        member this.Add(x: Bar) =
            if count > 0 then (data <- merge x) else (data <- x)
            count <- count + 1


    type private Buckets(size: int) =
        let mutable pos = 0
        let buckets = [| for _ in [1 .. size] do Bucket() |]
        let index k = (pos + k) % size

        member this.Debug(tag: string) =
            Log.Debug("DEBUG", $" ***** {tag} : pos = {pos} *****")
            for i in [1 .. size] do Log.Debug("DEBUG", $" {i}: {buckets[i-1].Count}")

        member this.Item with get(k: int) = buckets[index(k)]
        member this.Previous() = buckets[pos].Data.Epoch
        member this.Shift(k) =
            this.Debug($"Before Shift {k}")
            assert (k > 0)
            for i in [1 .. k] do buckets[index <| i-1].Reset()
            pos <- (pos + k) % size
            this.Debug($"After Shift {k}")

        member this.Reset() =
            pos <- 0 ; for i in [0 .. size - 1] do buckets[i].Reset()

    type Linear(interval: time, size: int) =

        let buckets = Buckets(size)
        let mutable previous: time = 0L
        let floor (t:time) = t - (t % (int64 interval))

        let extrapolate (diff: int) (output: Bar -> unit): unit =
            Log.Debug("Extrapolate", $"Extrapolate diff = {diff}")
#if DEBUG
            Debug.Assert(diff > 0)
            Debug.Assert(buckets[0].Count > 0, "First count is empty")
            Debug.Assert(buckets[diff].Count > 0, "Last count is empty")
            if diff > 1 then
                for k in [ 1 .. (diff - 1)] do
                    Debug.Assert(buckets[k].Count = 0, $"Bucket @ {k} is not empty")
#endif
            let prev = buckets[0].Data
            let curr = buckets[diff].Data
            let f (p: float, c: float) (r: float) = p + (c - p) * r

            for k in [1 .. diff] do
                let r = (float <| k-1) / (float diff)
                let dv = int64 <| (float <| (curr.Volume - prev.Volume)) * r
                // output <| Bar({| Open   = f(prev.Open, curr.Open) r
                let b =   Bar({| Open   = f(prev.Open, curr.Open) r
                                 Close  = f(prev.Close, curr.Close) r
                                 High   = f(prev.High, curr.High) r
                                 Low    = f(prev.Low, curr.Low) r
                                 Time   = previous + interval * (int64 <| k - 1)
                                 Volume = prev.Volume + dv |})
                Log.Debug("Extrapolate", $"Inserting {b}")
                output b

        interface Buffer with
            member this.Ingest(input: Bar, output: Bar -> unit): bool =
                let current = floor input.Epoch
                Log.Debug("Input", $" Previous = {previous} / Current = {current} -> {input}")
                Log.Debug("Debug", $"buckets[0].Count = {buckets[0].Count}")
                buckets.Debug(" -----------------------")
                if buckets[0].Count = 0 then
                    buckets[0].Add input
                    if input.Epoch % interval = 0 then output(input)
                    Log.Debug("Diff", $"Initial {input}")
                    previous <- current ; true
                else
                    Log.Debug("Debug", $"That input.Epoch = {input.Epoch}")
                    Log.Debug("Debug", $"That {input.Epoch} >= {buckets.Previous()}")
                    assert (input.Epoch >= buckets.Previous())
                    Log.Debug("Debug", "That")
                    let diff = int <| (current - previous) / interval
                    Log.Debug("Diff", $"Diff = {diff} -> {input}")
                    if diff >= size then (buckets.Reset() ; false) else
                        buckets[diff].Add input
                        if diff > 0 then
                            extrapolate diff output
                            buckets.Shift(diff)
                        previous <- current ; true
