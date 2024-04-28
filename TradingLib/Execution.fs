namespace TradingLib

open TradingLib


type Execution =
    abstract Welcome: string
    abstract StartTime: Maybe<System.DateTime>
    abstract EndTime: Maybe<System.DateTime>
    abstract InitialCapital: float
    abstract TargetCapital: float
    abstract StopLossCapital: float
    abstract Strategy: Strategy
    abstract Client: unit -> Client<'T>
    abstract Source: unit -> Data.Source
    abstract Delay: int
    abstract

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

    type Order(source: Data.Source, strategy: Strategy) =
        let state (ticker: Ticker) = State(source[ticker], strategy, source.Size)
        let map = Utils.CreateDictionary(source.Tickers, state)
        let get (order: Maybe<Order.Entry>) (ticker: Ticker) = map[ticker].Get(order)
        member this.Order() = source.Tickers |> List.fold get No


        match order with Yes(order) -> execute(order) | No -> Delay
        check

    type private loop(execution: Execution, source: Data.Source) =

        let input: Vector<Bar> = Vector.Buffer(source.BufferSize, fun _ -> Bar())
        let mutable previous =
            while not(sourceUtils.Elapsed start) do (Utils.Wait execution.Delay)


        [<TailCall>]
        let wait() =


    let private wait (start: System.DateTime) (delay: int) =
        let str = start.ToString("F")
        // Get Client and initialize strategy here!
        Log.Info("Execute", $"Waiting for Start time of {str}")
        while not(Utils.Elapsed start) do (Utils.Wait delay)

    let private run (execution: Execution) (source: Data.Source): bool =
        let info = client.AccountInfo()
        if info.Total < execution.InitialCapital then
            Log.Error("Main", $"Account doesn't have initial capital = {info.Total}")
        else
            match execution.StartTime with No -> ()
                                         | Yes(start) -> wait start execution.Delay


            while not(Utils.Elapsed start) do (Utils.Wait delay)
            let loop
                let start = param.StartTime.ToString("F")


                let strategy: Strategy  = param.StrategyFunction client
                if client.Stream(delay) then
                    Log.Info("Execute", "Streaming all data") ; strategy.Run(initial)
                else
                    Log.Error("Execute", "Trading bot failed to stream data!")

    let Run(execution: Execution): bool =
        Log.Info("Main", $" ****** {execution.Welcome} ***** ")
        Log.Info("Main", $"Trading bot starting at {Utils.CurrentTime()}")
        let source = execution.Source()
        let result: bool = try (run execution source) finally source.Dispose()
        Log.Info("Execute", $"Trading bot ended at {Utils.CurrentTime()}!")
        if result then true else false

(*

    let Main (param: struct {| StartTime: System.DateTime
                               EndTime: System.DateTime
                               ClientFunction: unit -> Client
                               StrategyFunction: Client -> Strategy |}): int =



*)
