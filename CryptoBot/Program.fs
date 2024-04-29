namespace CryptoBot

open TradingLib


module Program =

    let test() =
        let size = 10
        let data = Vector.Buffer(size, fun _ -> Bar())

        let tickers: Ticker list = [ Crypto("BTC") ]

        let source: Data.Source = Gemini.Source {|
            Tickers = tickers
            Size = size
            Buffer = Buffer.Linear(1L, 5)
            AskBidDifference = 0.15
            Timeout = 5 |}

        for i = 1 to 10 do
            printfn $" ***** {i} *****"
            let b = source.Data[Crypto("BTC")].Get(data)
            if b then
                for i in [1 .. size] do printfn "Output -> %A" data[i-1]
            else printfn "SKIP"
            Utils.Wait(5)

    [<EntryPoint>]
    let main _: int = test() ; 0
