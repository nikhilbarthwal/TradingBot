namespace CryptoBot

open TradingLib


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
