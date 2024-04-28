namespace CryptoBot

type DoubleMovingAverage(short: int, long: int) =
    member this.Short = short
    member this.Long = long
