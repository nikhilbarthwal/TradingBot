namespace TradingLib

open System.Text.Json


module Gemini =
    
    type private Parser(tag: string, difference: float) =
    
        let mutable bestAsk: Maybe<float> = No
        let mutable bestBid: Maybe<float> = No
        //let tag = $"Gemini[{symbol}]"

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

        let parse (message: string, insert: Bar -> unit): unit =
            let json: JsonElement = JsonDocument.Parse(message).RootElement
            try (processMessage json) with e ->
                Log.Error(tag, $"Unable to parse {message} -> {e.Message}")

            match bestAsk, bestBid with
            | Yes(ask), Yes(bid) -> getPrice ask bid <| getBar json insert
            | _ -> ()

    type private Adapter(z: {|
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


        interface Socket.Adapter.Multi with
            member this.Tickers = z.Tickers
            member this.Timeout = z.Timeout
            member this.Start _ = ()
            member this.Buffer = z.Buffer
            member this.Size = z.Size
            member this.Url(ticker) = url(ticker)
            member this.Receive(ticker, msg, insert) =
                parser[ticker].Parse(msg, insert)

            abstract Reconnect: Ticker * string -> unit
            abstract Send: Ticker * string -> unit
            abstract Tag: Ticker -> string


        let reconnect(msg: string) =
            Log.Warning(tag, $"Reconnecting for {symbol} -> {msg}")

        let socket: System.IDisposable = new Socket {|
            Url =
            Tag = tag
            Timeout = timeout
            Receive = parse
            Send = fun _ -> ()
            Reconnection = reconnect |}

        interface System.IDisposable with member this.Dispose() = socket.Dispose()

    type private Exchange(z: {|
            Tickers: Ticker list
            Size: int
            Preprocessor: Buffer
            AskBidDifference: float
            Timeout: int |}) =
        do
            for ticker in z.Tickers do
                match ticker with
                | Crypto _ -> ()
                | _ -> Log.Error("Gemini",
                                $"Gemini only supports Crypto, not {ticker}")

        let exchange = Data.Exchange(z.Tickers, z.Size, z.Preprocessor)
        let connection ticker: System.IDisposable = new Connection(
            z.AskBidDifference, exchange[ticker], ticker.Symbol,  z.Timeout)
        let connections = Utils.CreateDictionary(z.Tickers, connection)

        member this.Exchange = exchange
        member this.Dispose() =
            for ticker in z.Tickers do connections[ticker].Dispose()

        interface System.IDisposable with member this.Dispose() = this.Dispose()

    let Source z = Socket.Source.Multi(new Adapter(z))
