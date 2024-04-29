namespace TradingLib

open Alpaca.Markets


type Alpaca(apiKey: string, apiSecret: string) =
    let secret = SecretKey(apiKey, apiSecret)
    let client = Environments.Paper.GetAlpacaTradingClient(secret)
    let error (ticker: Ticker) =
        $"Currently, only Crypto is supported for Alpaca, not {ticker}"

    let accountInfo() =
        let result = client.GetAccountAsync().Result
        if result.IsTradingBlocked then
            Log.Error("Alpaca", "Trading for this account is blocked")

        let total = if result.BuyingPower.HasValue then
                        float <| result.BuyingPower.Value
                    else
                        Log.Error("Alpaca", "Unable to get total account value")

        let profit = if result.Equity.HasValue then
                         float <| result.Equity.Value - result.LastEquity
                     else
                         Log.Error("Alpaca", "Unable to get account equity value")

        { Total = total ; Profit = profit }

    let placeOrder(order: Order.Entry): IOrder =
        try
            let symbol = match order.Ticker with
                         | Crypto(sym) -> sym + "USD"
                         | ticker -> Log.Error("Alpaca", error ticker)

            let quantity: OrderQuantity = OrderQuantity.FromInt64(order.Quantity)
            let takeProfitLimitPrice = decimal <| order.Profit
            let stopLossStopPrice = decimal <| order.Loss
            let task = client.PostOrderAsync(LimitOrder.Buy(symbol, quantity,
                                     decimal <| order.Price).WithDuration(
                TimeInForce.Day).Bracket(takeProfitLimitPrice, stopLossStopPrice))
            task.RunSynchronously()
            task.Result
        with e -> Log.Exception("Alpaca", e.Message) e

    let cancelOrder(id: System.Guid): bool =
        try
            let task = client.CancelOrderAsync(id)
            task.RunSynchronously()
            task.Result
        with e -> Log.Exception("Alpaca", e.Message) e

    let orderStatus(id: System.Guid): Order.Status =
        try
            let task = client.GetOrderAsync(id)
            task.RunSynchronously()
            let iOrder = task.Result
            if iOrder.CancelledAtUtc.HasValue then Order.Status.Cancelled else
                if iOrder.FilledAtUtc.HasValue then Order.Status.Executed else
                    if iOrder.FilledQuantity > 0.0m then Order.Status.Triggered else
                        Order.Status.Placed
        with e -> Log.Exception("Alpaca", e.Message) e

    interface Client<System.Guid> with
        member this.AccountInfo(): AccountInfo = accountInfo()
        member this.PlaceOrder(order): System.Guid = placeOrder(order).OrderId
        member this.CancelOrder(id): bool = cancelOrder(id)
        member this.OrderStatus(id): Order.Status = orderStatus(id)
