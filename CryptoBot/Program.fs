namespace CryptoBot

open TradingLib
open System

type DoubleMovingAverage(short: int, long: int, quantity: uint) =
    do assert (long > short)
    member this.Size = long
    interface Strategy with
        member this.Execute(ticker, data): Maybe<Order.Entry> =
            assert (data.Size >= long)
            let price = data[0].Price
            let sum (s: float) (i:int): float = s + data[i-1].Price
            let longAvg = List.fold sum 0.0 [1 .. long]
            let shortAvg = List.fold sum 0.0 [1 .. short]
            if shortAvg <= longAvg * 1.01 then No else Yes <| Order.Entry({|
                Ticker = ticker
                Quantity = quantity
                Price = price
                Profit = price * 1.01
                Loss = price * 0.97 |})

module Program =

    type demo() =
        let size = 100
        let apiKey = Environment.GetEnvironmentVariable("ALPACA_API_KEY")
        let apiSecret = Environment.GetEnvironmentVariable("ALPACA_API_SECRET")
        let strategy = DoubleMovingAverage(size/2, size, 10u)
        let alpaca = Alpaca(apiKey, apiSecret) :> Client<Guid>
        let buffer = Buffer.Linear(1, 5)
        let source = Gemini.Source({| Tickers = [Crypto("BTC")]
                                      Size = size
                                      Buffer = buffer
                                      AskBidDifference = 0.5
                                      Timeout = 15 |})
        interface Execution<Guid> with
            member this.Welcome: string = "This is a demo"
            member this.StartTime = No
            member this.EndTime = No
            member this.InitialCapital = 1000.0
            member this.TargetCapital = 1000.0
            member this.StopLossCapital = 1000.0
            member this.Client(): Client<Guid> = alpaca
            member this.Strategy() = strategy
            member this.Source() = source
            member this.Delay = 5
            member this.Execute(order) = alpaca.PlaceOrder(order) |> ignore
                                         Utils.Wait(15)

    [<EntryPoint>]
    let main _: int = if (let x = demo() in Execution.Run(x)) then 1 else 0
