namespace TradingApp

open TradingLib

type Curve = abstract member Filter: int * Vector<Bar> * Vector.Buffer<Bar> -> float
             abstract member Iterations: int

module Program =

    let MaxError: float = 1.0

    let Best (c: Curve) (threshold: float)
             (output: Vector.Buffer<Bar>) (input:Vector<Bar>): bool =

        let rec iterate (k: int): bool =
            if (k > c.Iterations) then false else
                let e = c.Filter(k, input, output)
                if e > threshold then (iterate <| k + 1) else true
        iterate 1





    printfn "Hello World!"
