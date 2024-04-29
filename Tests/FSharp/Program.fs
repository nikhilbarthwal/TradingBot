namespace FilterTest

open System
open System.IO
open Trading


module Program =

    let private folder = "Temp"
    let private rnd = Random()
    let private minLength = 52
    let private maxLength = 200
    let private maxOrder = 7

    let private Content (i: int) (size:int) (v: int -> float): string =
        let b = i = size
        let z = String.concat ", " [|for i in 0 .. (size - 1) -> $"%.5f{v i}"|]
        if b then $"    [ {z} ]" else $"    [ {z} ],"

    let private Test (order: int) : string[] =
        let length = rnd.Next(minLength, maxLength)
        let window = rnd.Next(minLength - maxOrder - 1, length)
        let f = Maths.Filter.Compute(order, length, window)
        let m = f.Matrix order
        let header = [| "{" ;
                      $"  \"length\": {length}," ;
                      $"  \"window\": {window}," ;
                      $"  \"order\": {order}," ;
                       "  \"values\": [" |]
        let body = [| for i in [1.. m.Rows] do Content i m.Rows (m.Row <| i - 1) |]
        let footer = [| "  ]" ; "}" |]
        Array.concat [| header ; body ; footer |]

    [<EntryPoint>]
    let main _: int =
        for i in [1 .. 10] do
            for order in [1 .. maxOrder] do
                Log.Info("FilterTest", $"Conducting test {i}-{order} ...")
                File.WriteAllLines($"{folder}/test{i}-{order}.json", Test order)
        0
