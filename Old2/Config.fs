namespace CryptoBot

module Config =

    open System

    // Queue length for Data inside Buffer
    let BufferSize: int = 240

    // Interval for normalization (0.5s)
    let BufferInterval: time = 500

    // Wait for socket to close (in sec)
    let SocketCloseTimeout: int = 5

    // Wait for data to be ready (in sec)
    let ReadyWaitTime: int = 150

    // List of tickers for Gemini exchange
    let GeminiTickers: ticker list = [Crypto("ETH"); Crypto("BTC")]

    // Reset threshold for ticker (in ms)
    let ResetThresholdTime: time = 5000

    // Buffer size for Bucket Linearization
    let BucketSchemeBufferSize: int = 8

    // Default data ingestion scheme
    let IngestionScheme = BucketIngestion

    // Max % of difference between Ask & Bid to decide price
    let AskBidDifference: float = 0.5

    // Start and End time of the bot
    let StartTime: DateTime  = DateTime(2022, 8, 12, 9, 30, 0)
    let EndTime: DateTime = DateTime(2022, 8, 12, 15, 30, 0)

    // Max number of Iterations to execute
    let MaxIterations = 100

    // Delay between each iterations
    let Delay = 3

    // Time for order to execute
    let OrderExecutionTime = 1

    // Today's target
    let Target: target = {InitialCapital = 1000.0; TargetCapital = 1200.0; StopLossCapital = 800.0}
