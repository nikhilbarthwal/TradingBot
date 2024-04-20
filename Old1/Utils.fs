namespace Trading

open System
open System.Collections.Immutable
open System.Diagnostics


[<Struct>] type Maybe<'T> = Yes of 'T | No
    with static member (->>)(x: Maybe<'T>, f: 'T -> Maybe<'U>): Maybe<'U> =
            match x with Yes(y) -> f y | No -> No

[<Struct>] type Either<'U, 'V> = First of value1:'U | Second of value2:'V

module Utils =

    let ToDateTime(epoch: int64): DateTime =
        let dateTimeOffset  = DateTimeOffset.FromUnixTimeSeconds(epoch)
        let estZone = TimeZoneInfo.FindSystemTimeZoneById("Eastern Standard Time")
        TimeZoneInfo.ConvertTimeFromUtc(dateTimeOffset.DateTime, estZone)

    let Normalize(x: float) = Math.Round(2.0 * x, 1) / 2.0
    let CurrentTime() = DateTime.Now.ToString("F")
    let Wait (timeout: int) =
        assert (timeout > 0) ; Threading.Thread.Sleep(timeout * 1000)

    let inline Diff (a: float, b: float) = Normalize(100.0 * (a-b) / b)
    let inline Elapsed (time: DateTime) = DateTime.Now > time
    let inline Increment(original: float, increment: float): float =
            Normalize <| (original * (100.0 + increment)) / 100.0

    let Repeat (max: int) (f: unit -> bool): bool =
        assert (max > 1)
        let rec loop (i: int): bool =
            if (f ()) then true else
                if i = 5 * max then false else
                    Threading.Thread.Sleep(200) ; loop (i+1)
        in loop 1

    // let Between (z: float) (x: float, y: float): bool = (z >= x) && (y >= z)

    let CreateDictionary<'V, 'K when 'K: equality>(l: 'K list, f: 'K -> 'V) =
        let data = System.Collections.Generic.Dictionary<'K, 'V>(l.Length)
        (for x in l do data.Add(x, f x)) ; data.ToImmutableDictionary()


module Log =

    type private log() =
        do Trace.Listeners.Add(new ConsoleTraceListener(true)) |> ignore

        member this.Entry (header: string) (tag: string, msg: string): unit =
            let timestamp = DateTime.Now.ToString("yyyy-MM-dd HH:mm:ss.fff")
            Trace.WriteLine($"[{timestamp}] {header} {tag}: {msg}")

    let private logger = log()
    let Error(tag, msg) = logger.Entry "ERROR" (tag, msg) ; failwith $"{tag}: ${msg}"
    let Warning = logger.Entry "WARNING"
    let Info = logger.Entry "INFO"
    let Debug = logger.Entry "DEBUG"
