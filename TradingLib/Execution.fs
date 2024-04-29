namespace TradingLib

open TradingLib


type Execution =
    abstract Welcome: string
    abstract StartTime: Maybe<System.DateTime>
    abstract EndTime: Maybe<System.DateTime>
    abstract InitialCapital: float
    abstract TargetCapital: float
    abstract StopLossCapital: float
    abstract Strategy: unit -> Strategy
    abstract Client: unit -> Client<'T>
    abstract Source: unit -> Data.Source
    abstract Delay: int
    abstract Execute: Order.Entry -> unit

module Execution =

    type private State(source: Data, strategy: Strategy, size: int) =
        let mutable previous: time = 0L
        let data = Vector.Buffer(size, fun _ -> Bar())
        let get(): Maybe<Order.Entry> =
            if not (source.Get(data)) then No else
                let current = data[0].Epoch
                assert (current >= previous)
                if current = previous then No else previous <- current
                                                   strategy.Execute(data)

        let best: Maybe<Order.Entry> * Maybe<Order.Entry> -> _ = function
            | No, No -> No
            | No, Yes(o) -> Yes(o)
            | Yes(o), No -> Yes(o)
            | Yes(o1), Yes(o2) ->
                if o1.ProfitPercent() > o2.ProfitPercent() then Yes(o1) else Yes(o2)

        member this.Get(oldOrder: Maybe<Order.Entry>) = let newOrder = get() in
                                                        best(newOrder, oldOrder)

    type Orders(source: Data.Source, strategy: Strategy) =
        let state (t: Ticker) = State(source.Data[t], strategy, source.Size)
        let map = Utils.CreateDictionary(source.Tickers, state)
        let get (order: Maybe<Order.Entry>) (ticker: Ticker) = map[ticker].Get(order)
        member this.Get() = source.Tickers |> List.fold get No

    let private wait (start: System.DateTime) (delay: int) =
        let str = start.ToString("F")
        // Get Client and initialize strategy here!
        Log.Info("Execute", $"Waiting for Start time of {str}")
        while not(Utils.Elapsed start) do (Utils.Wait delay)

    let private run (execution: Execution) (source: Data.Source): bool =
        Log.Info("Main", "Initializing client ...")
        let client = execution.Client()
        let info = client.AccountInfo()
        if info.Total < execution.InitialCapital then
            Log.Error("Main", $"Account doesn't have initial capital = {info.Total}")
        else
            match execution.StartTime with No -> ()
                                         | Yes(start) -> wait start execution.Delay

            Log.Info("Main", "Initializing strategy ...")
            let strategy = execution.Strategy()
            let orders = Orders(source, strategy)
            let stop(): Maybe<bool> = Yes(true) // TODO

            let rec loop() = match stop() with
                             | Yes(b) -> b
                             | No -> let order = orders.Get()
                                     match order with
                                     | Yes(o) -> execution.Execute(o)
                                     | No -> Utils.Wait execution.Delay
                                     loop()
            loop()

    let Run(execution: Execution): bool =
        Log.Info("Main", $" ****** {execution.Welcome} ***** ")
        Log.Info("Main", $"Trading bot starting at {Utils.CurrentTime()}")
        let source = execution.Source()
        let result: bool = try (run execution source) finally source.Dispose()
        Log.Info("Execute", $"Trading bot ended at {Utils.CurrentTime()}!")
        if result then true else false
