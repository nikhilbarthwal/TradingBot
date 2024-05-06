namespace TradingLib

open System


[<Struct>] type OptionType = Call | Put
    with override this.ToString() = match this with Call -> "Call" | Put -> "Put"


type Ticker =
    | Stock of Symbol: string
    | Option of Symbol: string * Strike: float * Expiry: DateTime * Type: OptionType
    | Crypto of Symbol: string
    with
        override this.ToString() =
            match this with
            | Stock(symbol) ->
                $"Symbol: {symbol}"
            | Option(symbol, strike, expiry, direction) ->
                let str = expiry.ToString("yyyy-MM-dd")
                $"Symbol: {direction} {symbol} / Strike: {strike} / Expiry: {str}"
            | Crypto(symbol) ->
                $"Symbol: {symbol}"

        member this.Symbol =
            match this with
            | Stock(symbol) -> symbol
            | Option(symbol, _, _, _) -> symbol
            | Crypto(symbol) -> symbol


[<Struct>] type AccountInfo = { Total: float ; Profit: float }


module Order =

    [<Struct>]
    type Entry (param: struct {| Ticker: Ticker; Quantity: uint; Price: float
                                 Profit: float; Loss: float |}) =

        member this.Ticker = param.Ticker
        member this.Quantity: int = int param.Quantity
        member this.Price = assert (param.Price > 0) ; Utils.Normalize(param.Price)
        member this.Profit = Utils.Normalize(param.Profit)
        member this.Loss = Utils.Normalize(param.Loss)
        member this.ProfitPercent() = (100.0 * (this.Profit - this.Price))/this.Price
        member this.LossPercent() = (100.0 * (this.Price - this.Loss))/this.Price
        with override this.ToString() =
                $"Ticker: {this.Ticker} / Quantity: {this.Quantity} / Price: " +
                $"{this.Price} / ProfitPrice: {this.Profit} / LossPrice: {this.Loss}"

    [<Struct>] type Status = Placed | Triggered | Executed | Cancelled


[<Struct>] //TODO: This should be heap based, not struct
type Bar (param: struct {| Open: float; High: float; Low: float
                           Close: float; Time: time; Volume: int64 |}) =

    //TODO: Add a Valid parameter
    member this.Open = Utils.Normalize(param.Open)
    member this.High = Utils.Normalize(param.High)
    member this.Low = Utils.Normalize(param.Low)
    member this.Close = Utils.Normalize(param.Close)
    member this.Epoch = param.Time
    member this.Volume = param.Volume
    member this.Timestamp = Utils.ToDateTime(param.Time)
    member this.Price = Utils.Normalize((this.High + this.Low) / 2.0)
    override this.ToString() =
        let ts = this.Timestamp.ToString("F")
        $"Open: {this.Open} / High: {this.High} / Close: {this.Close} / Low: " +
        $"{this.Low} / Timestamp: {ts} / Epoch: {this.Epoch} / Volume: {this.Volume}"


type Client<'T> =
    abstract AccountInfo: unit -> AccountInfo
    abstract CancelOrder: 'T -> bool
    abstract OrderStatus: 'T -> Order.Status
    abstract PlaceOrder: Order.Entry -> 'T


type Strategy = abstract Execute: Ticker * Vector<Bar> -> Maybe<Order.Entry>
