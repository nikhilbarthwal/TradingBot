namespace TradingLib

open System
open System.Diagnostics
open System.Collections.Immutable


type time = int64
type Dictionary<'K,'V> = System.Collections.Immutable.ImmutableDictionary<'K,'V>


module Utils =

    let ToDateTime(epoch: int64): DateTime =
        let dateTimeOffset  = DateTimeOffset.FromUnixTimeSeconds(epoch)
        let estZone = TimeZoneInfo.FindSystemTimeZoneById("Eastern Standard Time")
        TimeZoneInfo.ConvertTimeFromUtc(dateTimeOffset.DateTime, estZone)

    let inline Normalize(x: float) = Math.Round(x, 3)
    let inline CurrentTime() = DateTime.Now.ToString("F")

    let CreateDictionary<'V, 'K when 'K: equality>(l: 'K list, f: 'K -> 'V) =
        let data = System.Collections.Generic.Dictionary<'K, 'V>(l.Length)
        (for x in l do data.Add(x, f x)) ; data.ToImmutableDictionary()

    let inline Wait (timeout: int) =
        assert (timeout > 0) ; Threading.Thread.Sleep(timeout * 1000)

    let inline Elapsed (time: DateTime) = DateTime.Now > time


module Log =

    type private log() =
        do Trace.Listeners.Add(new ConsoleTraceListener(true)) |> ignore

        member this.Entry (header: string) (tag: string, msg: string): unit =
            let timestamp = DateTime.Now.ToString("yyyy-MM-dd HH:mm:ss.fff")
            let tagStr = if tag = "" then "" else $" {tag}"
            Trace.WriteLine($"[{timestamp}] {header}{tagStr}: {msg}")

    let private logger = log()
    let Warning = logger.Entry "WARNING"
    let Info = logger.Entry "INFO"
    let Debug = logger.Entry "Debug"
    let Error(tag, msg) =
        logger.Entry "ERROR" (tag, msg) ; raise (Exception(msg))
    let Exception(tag, msg) (ex: exn) =
        logger.Entry "ERROR" (tag, msg) ; raise (Exception(msg, ex))
