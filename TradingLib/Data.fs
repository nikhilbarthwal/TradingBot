namespace TradingLib


type Data = abstract Get: Bar[] -> bool

module Data =

    type Store = abstract member Insert: Bar -> unit
                 abstract member Reset: unit -> unit

    type private Vault(size: int, buffer: Buffer) =
        let object = System.Object()
        let data = Vector.Circular(size, fun _ -> Bar())
        let insert bar = lock object (fun _ -> data.Insert(bar))
        let reset() = lock object (fun _ -> data.Reset())
        let ingest bar = buffer.Ingest(bar, insert)
        interface Store with
            member this.Insert(bar: Bar) = if not (ingest bar) then reset()
            member this.Reset() = reset()
        interface Data with
            member this.Get(l) =
               //TODO: Add this check (for i in l do (assert not(i.Empty))) ; r
               lock object (fun _ -> data.Get(l))

    type Exchange(tickers: Ticker list, size: int, buffer: Buffer) =
        let reader, writer =
            let map = Utils.CreateDictionary(tickers, fun _ -> Vault(size, buffer))
            (Utils.CreateDictionary(tickers, fun ticker -> map[ticker] :> Data),
             Utils.CreateDictionary(tickers, fun ticker -> map[ticker] :> Store))

        member this.Item with get(ticker: Ticker): Store = writer[ticker]
        member this.Data = reader
        member this.Tickers: Ticker list = tickers

    type Source =
        inherit System.IDisposable
        abstract Data: Dictionary<Ticker, Data>
        abstract Tickers: Ticker list
