namespace TradingLib

open System.Drawing
open System.Text.Json


module Gemini =

    type private Connection(difference: float, store: Data.Store,
                            symbol: string, timeout: int) =

        let mutable bestAsk: Maybe<float> = No
        let mutable bestBid: Maybe<float> = No
        let tag = $"Gemini[{symbol}]"

        let processEvent (event: JsonElement): unit =
            if (event.GetProperty("type").GetString() = "change" &&
                event.GetProperty("reason").GetString() = "place") then
                let price: float = float <| event.GetProperty("price").GetString()
                let side: string = event.GetProperty("side").GetString()

                if side = "ask" then
                    match bestAsk with
                    | Yes(ask) -> if price < ask then bestAsk <- Yes(price)
                    | No -> bestAsk <- Yes(price)

                if side = "bid" then
                    match bestBid with
                    | Yes(bid) -> if price > bid then bestBid <- Yes(price)
                    | No -> bestBid <- Yes(price)

        let processMessage (json: JsonElement): unit =
            if json.GetProperty("socket_sequence").GetInt64() > 0 then
                for k in [1 .. json.GetProperty("events").GetArrayLength()] do
                    processEvent <| json.GetProperty("events").Item(k - 1)

        let getPrice (ask: float) (bid: float) (insert: float -> unit) =
            if bid >= ask then insert bid else
                if ((100.0 * (ask - bid)) / bid) < difference then
                    insert <| (ask + bid) / 2.0

        let insertBar (json: JsonElement) (price : float): unit =
            let time: time = json.GetProperty("timestampms").GetInt64()
            bestAsk <- No ; bestBid <- No
            store += Bar {| Open = price; High = price; Low = price; Close = price
                            Time = time; Volume = -1 |}

        let parse (message: string): unit =
            let json: JsonElement = JsonDocument.Parse(message).RootElement
            try (processMessage json) with e ->
                Log.Error(tag, $"Unable to parse {message} -> {e.Message}")

            match bestAsk, bestBid with
            | Yes(ask), Yes(bid) -> getPrice ask bid <| insertBar json
            | _ -> ()

        let reconnect(msg: string) =
            Log.Warning(tag, $"Reconnecting for {symbol} -> {msg}")

        let socket: System.IDisposable = new Socket {|
            Url = $"wss://api.gemini.com/v1/marketdata/{symbol}USD"
            Tag = tag
            Timeout = timeout
            Receive = parse
            Send = fun _ -> ()
            Reconnection = reconnect |}

        interface System.IDisposable with member this.Dispose() = socket.Dispose()

    type private Exchange(z: {|
            Tickers: Ticker list
            Size: int
            Preprocessor: Preprocessor
            AskBidDifference: float |}) =
        do
            for ticker in z.Tickers do
                match ticker with
                | Crypto(symbol) -> ()
                | _ -> Log.Error("Gemini",
                                 $"Gemini only supports Crypto, not {ticker}")

        let exchange = Data.Exchange(z.Tickers, z.Size, z.Preprocessor)
        let connection ticker: System.IDisposable =
            new Connection(z.AskBidDifference,
                           exchange[ticker], )
        let connections = Utils.CreateDictionary(z.Tickers, connection)

        member this.Exchange = exchange
        member this.Dispose() =
            for ticker in z.Tickers do connections[ticker].Dispose()

        interface System.IDisposable with member this.Dispose() = this.Dispose()

    let Source z = let e = new Exchange(z) in new Data.Source(e.Exchange, e.Dispose)
