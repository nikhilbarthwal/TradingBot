namespace CryptoBot

open System.Collections.Immutable


[<Struct>] type Maybe<'T> = Yes of 'T | No
type time = int64
type Dictionary = System.Collections.Generic.Dictionary

module Utils =

    let preciion = 3

    let ToDateTime(epoch: int64): DateTime =
        let dateTimeOffset  = DateTimeOffset.FromUnixTimeSeconds(epoch)
        let estZone = TimeZoneInfo.FindSystemTimeZoneById("Eastern Standard Time")
        TimeZoneInfo.ConvertTimeFromUtc(dateTimeOffset.DateTime, estZone)

    let Normalize(x: float) = Math.Round(x, 1) / 2.0
    let CurrentTime() = DateTime.Now.ToString("F")

    let CreateDictionary<'V, 'K when 'K: equality>(l: 'K list, f: 'K -> 'V) =
        let data = System.Collections.Generic.Dictionary<'K, 'V>(l.Length)
        (for x in l do data.Add(x, f x)) ; data.ToImmutableDictionary()


open System
open System.Diagnostics

module Log =

    type private log() =
        do Trace.Listeners.Add(new ConsoleTraceListener(true)) |> ignore

        member this.Entry (header: string) (id: int, msg: string): unit =
            let timestamp = DateTime.Now.ToString("yyyy-MM-dd HH:mm:ss.fff")
            let idStr = (if id > 0 then $" %d{id}" else "")
            Trace.WriteLine($"[{timestamp}] {header}{idStr}: {msg}")

    let private logger = log()
    let Error(id, msg) = logger.Entry "ERROR" (id, msg) ; exit(1)
    let Warning = logger.Entry "WARNING"
    let Info = logger.Entry "INFO"

    let Debug (tag: string,msg: string) = logger.Entry "DEBUG" (0, $"{msg}   @{tag}")
