namespace CryptoBot

type Date = {month: int ; day:int ; year: int } with
    override this.ToString() = $"{this.year}{this.month}{this.day}"

type Direction = Call | Put with
    override this.ToString() = match this with Call -> "Call" | Put -> "Put"

type ticker =
    | Crypto of symbol: string
    | Stock of symbol: string
    | Option of symbol: string * expiry: Date * strike: float * direction: Direction
    override this.ToString(): string =
        match this with
        | Crypto(symbol) -> symbol
        | Stock(symbol) -> symbol
        | Option(symbol, expiry, strike, direction) ->
            $"{symbol} {direction} {expiry} {strike}"

type time = int64

type tick = {Price: float; Time: time} with
    override this.ToString() = $"{this.Price} @ {this.Time}"

type order(ticker: ticker, price: float, quantity: int, profit: float, loss: float) =
    let id: string = System.Guid.NewGuid().ToString()
    member this.Id = id
    member this.Symbol = ticker.ToString()
    member this.Price = price
    member this.Quantity = quantity
    member this.UpperPrice = price * (1.0 + (profit / 100.0))
    member this.LowerPrice = price * (1.0 - (loss / 100.0))

type IngestionScheme = LinearIngestion | BucketIngestion

type Exchange =
    abstract member Tickers: ticker list
    abstract member Get: ticker -> float[] -> bool
    inherit System.IDisposable

type Broker =
    abstract member PlaceOrder: order -> unit
    abstract member OrderComplete: order -> bool
    abstract member CancelOrder: order -> bool
    abstract member Gains: order -> float
    inherit System.IDisposable

type target = {InitialCapital: float; TargetCapital: float; StopLossCapital: float} with
    override this.ToString() = $"InitialCapital = {this.InitialCapital} / "
                             + $"TargetCapital = {this.TargetCapital} / "
                             + $"StopLossCapital = {this.StopLossCapital}"

type Strategy = abstract member Run: int -> bool option
