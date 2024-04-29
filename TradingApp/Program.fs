namespace TradingApp

open TradingLib

type Curve = abstract member Filter: int * Vector<Bar> * Vector.Buffer<Bar> -> bool
             abstract member Iterations: int

module Program =
    printfn "Hello World!"
