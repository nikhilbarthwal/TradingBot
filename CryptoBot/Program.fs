open TradingLib
open TradingLib.Data


let size = 10
let data = [| for _ in 1 .. size -> Bar() |]
let tickers: Ticker list = [ Crypto("ETH") ; Crypto("BTC") ]

let source: Source = Gemini.Source {|
    Tickers = tickers
    Size = size
    Buffer = Buffer.Linear(1L, 5)
    AskBidDifference = 1.0
    Timeout = 5 |}

for i = 1 to 100 do
    let b = source.Data[Crypto("BTC")].Get(data)
    if b then (for d in data do printfn "%A" d) else printfn "SKIP"
    Utils.Wait(5)
