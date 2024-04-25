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


[<Struct>] type AccountInfo = { Total: float ; Cash: float ; Profit: float }


module Order =

    [<Struct>]
    type Entry (param: struct {| Ticker: Ticker; Quantity: uint; Price: float
                                 Profit: float; Loss: float |}) =

        member x.Ticker = param.Ticker
        member x.Quantity: int = int param.Quantity
        member x.Price = Utils.Normalize(param.Price)
        member x.Profit = Utils.Normalize(param.Profit)
        member x.Loss = Utils.Normalize(param.Loss)
        with override this.ToString() =
                $"Ticker: {this.Ticker} / Quantity: {this.Quantity} / Price: " +
                $"{this.Price} / ProfitPrice: {this.Profit} / LossPrice: {this.Loss}"

    [<Struct>] type Status = Placed | Triggered | Executed of bool | Cancelled


[<Struct>]
type Bar (param: struct {| Open: float; High: float; Low: float
                           Close: float; Time: time; Volume: int64 |}) =

    member this.Valid = true
    member this.Open = Utils.Normalize(param.Open)
    member this.High = Utils.Normalize(param.High)
    member this.Low = Utils.Normalize(param.Low)
    member this.Close = Utils.Normalize(param.Close)
    member this.Epoch = param.Time
    member this.Volume = param.Volume
    member this.Timestamp = Utils.ToDateTime(param.Time)
    member this.Price = (this.High + this.Low) / 2.0
    override this.ToString() =
        let ts = this.Timestamp.ToString("F")
        $"Open: {this.Open} / High: {this.High} / Close: {this.Close} / Low: " +
        $"{this.Low} / Timestamp: {ts} / Epoch: {this.Epoch} / Volume: {this.Volume}"


type target = {Initial: float; Target: float; StopLoss: float} with
    override this.ToString() = $"InitialCapital: {this.Initial} / "
                             + $"TargetCapital: {this.Target} / "
                             + $"StopLossCapital: {this.StopLoss}"


type Client =
    inherit IDisposable
    abstract CancelOrder: Ticker -> bool
    abstract CancelAllOrders: unit -> bool
    abstract GetAccountInfo: unit -> AccountInfo
    // abstract GetPositions: unit -> Maybe<Dictionary<Ticker, int * float>>
    // abstract Get: Ticker -> Maths.Array<Bar> -> bool
    abstract OrderStatus: Ticker -> Order.Status
    abstract Stream: int -> bool
    abstract PlaceOrder: Order.Entry -> bool
    abstract ResetOrders: unit -> unit
    abstract VerifyPlacedOrders: unit -> bool
    abstract VerifyCompletedOrders: unit -> bool


type Strategy = abstract Run: float -> bool
