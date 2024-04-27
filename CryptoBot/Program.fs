open TradingLib
open TradingLib.Data


let size = 10
let data = [| for _ in 1 .. size -> Bar() |]
let tickers: Ticker list = [ Crypto("BTC") ] // ; Crypto("BTC") ]

let source: Source = Gemini.Source {|
    Tickers = tickers
    Size = size
    Buffer = Buffer.Linear(1L, 5)
    AskBidDifference = 0.15
    Timeout = 5 |}

for i = 1 to 100 do
    printfn $" ***** {i} *****"
    let b = source.Data[Crypto("BTC")].Get(data)
    if b then (for d in data do printfn "Output -> %A" d) else printfn "SKIP"
    Utils.Wait(5)
