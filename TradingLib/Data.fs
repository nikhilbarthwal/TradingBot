namespace TradingLib


type Data = abstract Get: Bar[] -> bool

module Data =

    type private CircularArray(size: int) =
        let mutable pos: int = 0
        let mutable count: int = 0
        let data: Bar[] = [| for _ in 1 .. size -> Bar() |]

        member this.Insert(bar: Bar) = data[pos] <- bar ; count <- count + 1
                                       pos <- pos + 1 ; if pos = size then pos <- 0

        member this.Reset() = count <- 0 ; pos <- 0

        member this.Get(buffer: Bar[]): bool =
            if count < size then false else
                for index in [1 .. size] do
                    let n = (size + pos - index) % size
                    buffer[index - 1] <- data[n]
                true

    type IStore = abstract member Insert: Bar -> unit
                  abstract member Reset: unit -> unit

    type private Store(size: int, preprocessor: Preprocessor) =
        let object = System.Object()
        let data = CircularArray(size)
        let insert(bar) = lock object (fun _ -> data.Insert(bar))
        let reset() = lock object (fun _ -> data.Reset())
        interface IStore with
            member this.Insert(bar: Bar) =
                if not(preprocessor.Insert insert bar) then reset()
            member this.Reset() = reset()
        interface Data with
            member this.Get(l) = lock object (fun _ -> data.Get(l))

    type Exchange(tickers: Ticker list, size: int, preprocessor: Preprocessor) =
        let reader, writer =
            let map = Utils.CreateDictionary(tickers, fun _ -> Store(size, preprocessor))
            (Utils.CreateDictionary(tickers, fun ticker -> map[ticker] :> Data),
             Utils.CreateDictionary(tickers, fun ticker -> map[ticker] :> IStore))

        member this.Item with get(ticker: Ticker): IStore = writer[ticker]
        member this.Data = reader
        member this.Tickers: Ticker list = tickers

    type Source =
        inherit System.IDisposable
        abstract Data: Dictionary<Ticker, Data>
        abstract Tickers: Ticker list
