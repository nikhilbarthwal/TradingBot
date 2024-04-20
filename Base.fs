namespace Trading

open System


[<Struct>] type OptionType = Call | Put


type Ticker =
    | Stock of Symbol: string
    | Option of Symbol: string * Strike: float * Expiry: DateTime * Type: OptionType
    | Crypto of Symbol: string
    with
        override this.ToString() =
            match this with
            | Stock(symbol) ->
                $"Symbol = {symbol}"
            | Option(symbol, strike, expiry, direction) ->
                let str = expiry.ToString("yyyy-MM-dd")
                $"Symbol = {direction} {symbol} / Strike = {strike} / Expiry = {str}"
            | Crypto(symbol) ->
                $"Symbol = {symbol}"

        member this.Symbol =
            match this with
            | Stock(symbol) -> symbol
            | Option(symbol, _, _, _) -> symbol
            | Crypto(symbol) -> symbol


[<Struct>] type AccountInfo = { Total: float ; Cash: float ; Profit: float }


module Order =

    [<Struct>] type Entry (param: struct {| Ticker: Ticker; Quantity: uint; Price:
                                            float; Profit: float; Loss: float |}) =

        member x.Ticker = param.Ticker
        member x.Quantity: int = int param.Quantity
        member x.Price = Utils.Normalize(param.Price)
        member x.Profit = Utils.Normalize(param.Profit)
        member x.Loss = Utils.Normalize(param.Loss)
        with override this.ToString() =
                $"{this.Ticker} / Quantity = {this.Quantity} / Price = {this.Price}"
              + $" / Profit Price = {this.Profit} / Loss Price = {this.Loss}"

    [<Struct>] type Status = Placed | Triggered | Executed of bool | Cancelled


[<Struct>] type Bar (param: struct { Open: float; High: float; Low: float;
                                     Close: float; Time: time; Volume: int64 }) =

    member x.Open = Utils.Normalize(param.o)
    member x.High = Utils.Normalize(param.h)
    member x.Low = Utils.Normalize(param.l)
    member x.Close = Utils.Normalize(param.c)
    member x.Epoch = param.e
    member x.Volume = param.v
    member this.Price = (this.High + this.Low) / 2.0
    override this.ToString() =
        $"Timestamp = {DateTimeOffset.FromUnixTimeSeconds(this.Epoch).DateTime}" +
        $" / Epoch = {this.Epoch} / Open = {this.Open} / High = {this.High} / " +
        $"Close = {this.Close} / Low = {this.Low} / Volume = {this.Volume}"


type target = {Initial: float; Target: float; StopLoss: float} with
    override this.ToString() = $"InitialCapital = {this.InitialCapital} / "
                             + $"TargetCapital = {this.TargetCapital} / "
                             + $"StopLossCapital = {this.StopLossCapital}"


type Client =
    inherit IDisposable
    abstract CancelOrder: Ticker -> bool
    abstract CancelAllOrders: unit -> bool
    abstract GetAccountInfo: unit -> AccountInfo
    abstract GetPositions: unit -> Maybe<ImmutableDictionary<Ticker, int * float>>
    abstract Get: Ticker -> Maths.Array<Bar> -> bool
    abstract OrderStatus: Ticker -> Order.Status
    abstract Stream: int -> bool
    abstract PlaceOrder: Order.Entry -> bool
    abstract ResetOrders: unit -> unit
    abstract VerifyPlacedOrders: unit -> bool
    abstract VerifyCompletedOrders: unit -> bool


type Strategy = abstract Run: float -> bool
