namespace CryptoBot

open TradingLib


type DoubleMovingAverage(short: int, long: int) =
    interface Strategy with member this.Execute(_) = No
