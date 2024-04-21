namespace TradingLib

open System.Collections.Generic
open System.Text.Json


type Gemini(tickers: ticker list) =
    let data = Utils.CreateMap tickers Buffer
    let mutable bestAsk = Dictionary<string, float option>()
    let mutable bestBid = Dictionary<string, float option>()

    let processEvent (symbol: string) (event: JsonElement): unit =
        if (event.GetProperty("type").GetString() = "change" &&
            event.GetProperty("reason").GetString() = "place") then
            let price: float = float <| event.GetProperty("price").GetString()
            let side: string = event.GetProperty("side").GetString()

            if side = "ask" then
                match bestAsk[symbol] with
                | Some(ask) -> if price < ask then bestAsk[symbol] <- Some(price)
                | None -> bestAsk[symbol] <- Some(price)

            if side = "bid" then
                match bestBid[symbol] with
                | Some(bid) -> if price > bid then bestBid[symbol] <- Some(price)
                | None -> bestBid[symbol] <- Some(price)

    let processMessage (symbol: string) (json: JsonElement): unit =
        if json.GetProperty("socket_sequence").GetInt64() > 0 then
            for pos in [1 .. json.GetProperty("events").GetArrayLength()] do
                processEvent symbol <| json.GetProperty("events").Item(pos - 1)

    let getPrice (ask: float) (bid: float): float option =
        if bid >= ask then (Some bid) else
            let equal = ((100.0 * (ask - bid)) / bid) < Config.AskBidDifference
            if equal then Some((ask + bid) / 2.0) else None

    let getTick (symbol: string) (json: JsonElement) (price : float): tick =
        let time: time = json.GetProperty("timestampms").GetInt64()
        bestAsk[symbol] <- None ; bestBid[symbol] <- None
        {Price = Utils.Normalize price ; Time = time}

    let parse (symbol: string) (message: string): tick option =
        let json: JsonElement = JsonDocument.Parse(message).RootElement
        try (processMessage symbol json) with e ->
            Log.Error(0, $"Unable to parse {message} -> {e.Message}")

        match bestAsk[symbol], bestBid[symbol] with
        | Some(ask), Some(bid) -> getPrice ask bid
        | _ -> None
        |> Option.map (getTick symbol json)

    let createSocket (ticker: ticker): System.IDisposable =
        let symbol: string = match ticker with
                             | Crypto(symbol) -> symbol
                             | _ -> Log.Error(0, "Gemini only accepts Crypto")
        bestAsk.Add(symbol, None) ; bestBid.Add(symbol, None)
        let url: string = $"wss://api.gemini.com/v1/marketdata/{symbol}USD"
        new Socket(url, ticker, (parse symbol) >> data[ticker].Update)

    let sockets = Utils.CreateMap tickers createSocket

    interface Exchange with
        member this.Tickers = tickers
        member this.Get (ticker:ticker) (buffer:float[]) = data[ticker].Get(buffer)
        member this.Dispose() = for socket in sockets.Values do socket.Dispose()
