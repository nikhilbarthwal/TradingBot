namespace TradingLib


type Data = abstract Get: Bar[] -> bool

module Data =

    type private CircularArray(size: int) =
        let mutable pos: int = 0
        let mutable count: int = 0
        let data: Bar[] = [| for _ in 1 .. size -> Bar() |]

        member this.Insert(bar: Bar) = data[pos] <- bar ; count <- count + 1
                                       pos <- pos + 1 ; if pos = size then pos <- 0

        member this.Get(buffer: Bar[]): bool =
            if count < size then false else
                for index in [1 .. size] do
                    let n = (size + pos - index) % size
                    buffer[index - 1] <- data[n]
                true

    type IStore = abstract member Insert: Bar -> unit

    type private Store (size: int) =
        let lockObj = System.Object()
        let data = CircularArray(size)
        interface IStore with
            member this.Insert(b: Bar) = lock lockObj (fun _ -> data.Insert(b))
        interface Data with
            member this.Get(l) = lock lockObj (fun _ -> data.Get(l))

    type Buffer(buffer: IStore, preprocessor: Preprocessor) =
        member this.Insert = preprocessor.Insert buffer.Insert
        static member (+=) (store: Buffer, bar: Bar) = store.Insert(bar)

    type Exchange(tickers: Ticker list, size: int, pre: Preprocessor) =
        let reader, writer =
            let map = Utils.CreateDictionary(tickers, fun _ -> Store(size))
            (Utils.CreateDictionary(tickers, fun ticker -> map[ticker] :> Data),
             Utils.CreateDictionary(tickers, fun ticker -> Buffer(map[ticker], pre)))

        member this.Item with get(ticker: Ticker): Buffer = writer[ticker]
        member this.Data = reader
        member this.Tickers: Ticker list = tickers

    type Source(exchange: Exchange, destructor: unit -> unit) =
        member this.Data = exchange.Data
        member this.Tickers: Ticker list = exchange.Tickers
        interface System.IDisposable with member this.Dispose() = destructor()
