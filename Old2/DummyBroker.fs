namespace CryptoBot

type DummyBroker() =

    let random (): bool =
        let rnd = System.Random()
        let x = rnd.Next(1, 1000 * 1000 * 1000)
        x > 300 * 1000 * 1000

    let mutable profits: Map<string, float> = Map.empty

    interface Broker with
        member this.PlaceOrder(order: order): unit =
            let sellPrice =  if (random ()) then order.UpperPrice else order.LowerPrice
            let quantity = float <| order.Quantity
            profits <- profits.Add(order.Id, (sellPrice - order.Price) * quantity)
        member this.OrderComplete(order: order): bool = true
        member this.CancelOrder(order: order) = true
        member this.Gains(order: order): float = profits[order.Id]
        member this.Dispose() = ()
