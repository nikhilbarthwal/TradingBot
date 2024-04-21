namespace TradingLib

open System
open System.Diagnostics
open System.Collections.Immutable


[<Struct>] type Maybe<'T> = Yes of 'T | No
type time = int64
type Dictionary<'K,'V> = System.Collections.Immutable.ImmutableDictionary<'K,'V>

module Utils =

    let private precision = 3

    let ToDateTime(epoch: int64): DateTime =
        let dateTimeOffset  = DateTimeOffset.FromUnixTimeSeconds(epoch)
        let estZone = TimeZoneInfo.FindSystemTimeZoneById("Eastern Standard Time")
        TimeZoneInfo.ConvertTimeFromUtc(dateTimeOffset.DateTime, estZone)

    let Normalize(x: float) = Math.Round(x, precision) / 2.0
    let CurrentTime() = DateTime.Now.ToString("F")

    let CreateDictionary<'V, 'K when 'K: equality>(l: 'K list, f: 'K -> 'V) =
        let data = System.Collections.Generic.Dictionary<'K, 'V>(l.Length)
        (for x in l do data.Add(x, f x)) ; data.ToImmutableDictionary()


type Vector<'T> =
    abstract Size: int
    abstract Item : int -> 'T with get

module Vector =

    type Buffer<'T> (size: int, f: int -> 'T) =
        let data: 'T[] = [| for i in 1 .. size -> f (i - 1) |]
        member this.Item with get(i: int) = data[i]
        member this.Size: int = size

        member this.Overwrite(f: int -> 'T): unit =
            for i in 0 .. (size-1) do data[i] <- (f i)

        interface Vector<'T> with
            member this.Item with get(i: int) = data[i]
            member this.Size = size

    let Create<'T>(size: int) (f: int -> 'T): Vector<'T> = Buffer(size, f)

    let Convolve<'T when 'T :> System.Numerics.IAdditionOperators<'T, 'T, 'T>
                     and 'T :> System.Numerics.IMultiplyOperators<'T, 'T, 'T>>
           (l: int) (f1: int -> 'T, f2: int -> 'T): 'T =
        let f (sum: 'T) k = sum + (f1 k) * (f2 k)
        let init = (f1 0) * (f2 0) in (List.fold f init [1 .. l - 1])


module Log =

    type private log() =
        do Trace.Listeners.Add(new ConsoleTraceListener(true)) |> ignore

        member this.Entry (header: string) (tag: string, msg: string): unit =
            let timestamp = DateTime.Now.ToString("yyyy-MM-dd HH:mm:ss.fff")
            let tagStr = if tag = "" then "" else $" {tag}"
            Trace.WriteLine($"[{timestamp}] {header}{tagStr}: {msg}")

    let private logger = log()
    let Error(id, msg) = logger.Entry "ERROR" (id, msg) ; exit(1)
    let Warning = logger.Entry "WARNING"
    let Info = logger.Entry "INFO"
    let Debug = logger.Entry "Debug"
