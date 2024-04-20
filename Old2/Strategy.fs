namespace CryptoBot

type MeanRevision(target: target, exchange: Exchange, broker: Broker) =
    let data: float[] = Utils.NewArray Config.BufferSize -1.0
    let shortInterval = 10
    let longInterval = 200
    let ticker: ticker = Config.GeminiTickers[0]
    let mutable balance = target.InitialCapital

    do Log.Info(0, $"Setting up Mean Revision Strategy with Target: {target}")

    let getOrder(iter: int): order option =
        let price:float = data[0]
        let quantity = int <| balance / price
        let longAvg = Utils.Normalize  (Array.average data[.. longInterval])
        let shortAvg = Utils.Normalize (Array.average data[.. shortInterval])
        let difference = longAvg - shortAvg
        Log.Info(iter, $"Found {shortInterval} time unit MVA = {shortAvg} and " +
                    $"Found {longInterval} time unit MVA = {longAvg}")
        if difference > 0.0 (* Usually a threshold here *) then
            let percent = Utils.Percent (longAvg, shortAvg)
            Some (order(ticker, price, quantity, percent, 1.5 * percent))
        else None

    interface Strategy with
        member this.Run (iter: int): bool option =
            let fetch = exchange.Get ticker data
            if (not fetch) then (Log.Info(iter, "Unable to fetch data") ; None) else
                match (getOrder iter) with
                | None -> Log.Info(iter, "No order found") ; None
                | Some(order) ->
                    broker.PlaceOrder(order)
                    Utils.Wait Config.OrderExecutionTime
                    if broker.OrderComplete(order) then
                        balance <- balance + broker.Gains(order)
                    else
                        Log.Error(iter, $"Order {order.Id} stuck!")
                    if balance >= target.TargetCapital then Some(true) else
                        if balance <= target.StopLossCapital then Some(false) else None
