namespace TradingLib


type Data = abstract Get: Bar[] -> bool

module Data =

    type Source =
        abstract Tickers: Ticker list
        abstract Data: Dictionary<Ticker, Data>

    type private CircularArray(size: int) =
        let mutable pos: int = 0
        let mutable count: int = 0
        let data = [| for _ in 1 .. size -> Bar() |]

        member this.Insert(bar: Bar) = data[pos] <- bar ; count <- count + 1
                                       pos <- pos + 1 ; if pos = size then pos <- 0

        member this.Get(buffer: Bar[]): bool =
            if count < size then false else 
                for index in [1 .. size] do
                    let n = (size + pos - index) % size
                    buffer[index - 1] <- data[n]
                true

    type private Buffer (size: int) =
        let lockObj = System.Object()
        let data = CircularArray(size)
        member this.Insert(b: Bar) = lock lockObj (fun _ -> data.Insert(b))
        interface Data with member this.Get(l) = lock lockObj (fun _ -> data.Get(l))

    type Store(buffer: Buffer, preprocessor: Preprocessor) =
        let preprocess = preprocessor buffer.Insert 
        member this.Insert(bar: Bar) = preprocess.Insert(bar)
        static member (+=) (store: Store, bar: Bar) = store.Insert(bar)

    type Exchange(tickers: Ticker list, size: int, pre: Preprocessor) =
        let reader, writer =
            let map = Utils.CreateDictionary(tickers, fun ticker -> Buffer(size))
            (Utils.CreateDictionary(tickers, fun ticker -> map[ticker] :> Data),
             Utils.CreateDictionary(tickers, fun ticker -> Store(map[ticker], pre)))

        member this.Item with get(ticker: Ticker): Store = writer[ticker]
        interface Source with member this.Data = reader
                              member this.Tickers: Ticker list = tickers
