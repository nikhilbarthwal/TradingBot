namespace Trading

open System
open System.Collections.Immutable


[<Struct>] type Type = Call | Put

type Ticker =
    | Stock of Symbol: string
    | Option of Symbol: string * Strike: float * Expiry: DateTime * Direction: Type
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


[<Struct>] type AccountInfo =
                { Total: Maybe<float> ; Cash: Maybe<float> ; Profit: Maybe<float> }


module Order =

    [<Struct>]
    type Entry (param: struct {| Ticker: Ticker; Quantity: uint
                                 Price: float; Profit: float; Loss: float |}) =

        member x.Ticker = param.Ticker
        member x.Quantity: int = int param.Quantity
        member x.Price = Utils.Normalize(param.Price)
        member x.Profit = Utils.Normalize(param.Profit)
        member x.Loss = Utils.Normalize(param.Loss)
        with override this.ToString() =
                $"{this.Ticker} / Quantity = {this.Quantity} / Price = {this.Price}"
              + $" / Profit Price = {this.Profit} / Loss Price = {this.Loss}"

    [<Struct>]
    type Status = Placed | Triggered | Executed of bool | Cancelled | Unknown


[<Struct>]
type Bar (param: struct {| o: float; h: float; l: float
                           c: float; e: int64; v: int64 |}) =

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

    static member Array(size: int) = Maths.Array.Create<Bar> size <| fun _ -> Bar()


type Circular<'T> (size: int, f: int -> 'T) =

    let data: 'T[] = [| for i in 1 .. size -> f i |]
    let mutable count = 0
    member this.Count(): int = count
    member this.Append(x: 'T) =  data[count % size] <- x ; count <- count + 1

    interface Trading.Maths.Array<'T> with
        member this.Item with get(i: int): 'T = data[(count - 1 - i) % size]
        member this.Size = size

type Buffer(size: int) =

    let data = Circular(size, fun _ -> Bar())
    let array: Maths.Array<Bar> = data
    let lck = Object()
    let get (i: int) = array[i]

    member this.Append(b: Bar) = lock lck (fun _ -> data.Append(b))
    member this.Count() = lock lck (fun _ -> data.Count())
    (* member this.Get(v: Maths.Vector<Bar>) =
        assert (v.Size = size)
        lock lck (fun _ -> if data.Count() < size then false
                                                  else v.Overwrite(get) ; true) *)

    static member (+=) (b: Buffer, param) = b.Append(param)


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
