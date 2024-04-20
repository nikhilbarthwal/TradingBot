namespace CryptoBot



module Program =

    let exchange (): Exchange = new Gemini(Config.GeminiTickers)
    let broker (): Broker = new DummyBroker()
    let Strategy exchange broker: Strategy = MeanRevision(Config.Target, exchange, broker)

    [<EntryPoint>]
    let main _ =

        let execute (exchange: Exchange) (broker: Broker): bool =
            let strategy = Strategy exchange broker
            Utils.Repeat Config.MaxIterations Config.Delay Config.EndTime <| strategy.Run

        let start (exchange: Exchange): bool =
            Log.Info(0, $"Waiting to fill buffers (Approx. {Config.ReadyWaitTime} secs.)")
            Utils.Wait(Config.ReadyWaitTime)
            using (broker ()) (execute exchange)

        let startTime = System.DateTime.Now.ToString("F")
        Log.Info(0, $"Trading bot starting at {startTime}")
        Utils.WaitUntil Config.StartTime
        let result: bool = using (exchange ()) start
        let endTime = System.DateTime.Now.ToString("F")
        if not result then Log.Warning(0, "Target not achieved!")
        Log.Info(0, $"Trading bot ends at {endTime}")
        if result then 0 else 1
