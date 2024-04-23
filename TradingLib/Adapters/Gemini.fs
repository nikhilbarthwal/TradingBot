namespace TradingLib

open System.Text.Json


module Gemini =

    type private Parser(tag: string, difference: float, symbol: string) =

        let mutable bestAsk: Maybe<float> = No
        let mutable bestBid: Maybe<float> = No

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

        let getBar (json: JsonElement) (insert: Bar -> unit) (price : float): unit =
            let time: time = json.GetProperty("timestampms").GetInt64()
            bestAsk <- No ; bestBid <- No
            insert <| Bar {| Open = price; High = price; Low = price; Close = price
                             Time = time; Volume = -1 |}

        member this.Parse(message: string, insert: Bar -> unit): unit =
            let json: JsonElement = JsonDocument.Parse(message).RootElement
            try (processMessage json) with e ->
                Log.Error(tag, $"Unable to parse {message} -> {e.Message}")

            match bestAsk, bestBid with
            | Yes(ask), Yes(bid) -> getPrice ask bid <| getBar json insert
            | _ -> ()


    let Source(z: {|
            Tickers: Ticker list
            Size: int
            Buffer: Buffer
            AskBidDifference: float
            Timeout: int |}) =

        let symbol (ticker: Ticker): string =
            match ticker with
            | Crypto(symbol) -> symbol
            | _ -> Log.Error("Gemini", $"Gemini only supports Crypto, not {ticker}")

        let symbols = Utils.CreateDictionary(z.Tickers, symbol)
        let url ticker = $"wss://api.gemini.com/v1/marketdata/{symbols[ticker]}USD"
        let tags = Utils.CreateDictionary(z.Tickers,
                                          fun ticker -> $"Gemini[{symbols[ticker]}]")
        let parser ticker = Parser(tags[ticker], z.AskBidDifference, symbols[ticker])
        let parsers = Utils.CreateDictionary(z.Tickers, parser)

        let reconnect(ticker: Ticker, msg: string) =
            Log.Warning(tags[ticker], $"Reconnecting for {symbols[ticker]} -> {msg}")

        Socket.Source.Multi({new Socket.Adapter.Multi with
            member this.Tickers = z.Tickers
            member this.Timeout = z.Timeout
            member this.Start _ = ()
            member this.Buffer = z.Buffer
            member this.Size = z.Size
            member this.Url(ticker) = url(ticker)
            member this.Tag(ticker) = tags[ticker]
            member this.Reconnect(ticker, msg) = reconnect(ticker, msg)
            member this.Send(_, _) = ()
            member this.Dispose() = ()
            member this.Receive(ticker, msg, insert) =
                parsers[ticker].Parse(msg, insert) })
