namespace Trading.Maths


type Array<'T> =
    abstract Size: int
    abstract Item : int -> 'T with get

module Array =

    type Buffer<'T> (size: int, f: int -> 'T) =
        let data: 'T[] = [| for i in 1 .. size -> f (i - 1) |]
        member this.Item with get(i: int) = data[i]
        member this.Size: int = size

        member this.Overwrite(f: int -> 'T): unit =
            for i in 0 .. (size-1) do data[i] <- (f i)

        interface Array<'T> with
            member this.Item with get(i: int) = data[i]
            member this.Size = size

    let Create<'T>(size: int) (f: int -> 'T): Array<'T> = Buffer(size, f)

    let Convolve<'T when 'T :> System.Numerics.IAdditionOperators<'T, 'T, 'T>
                     and 'T :> System.Numerics.IMultiplyOperators<'T, 'T, 'T>>
           (l: int) (f1: int -> 'T, f2: int -> 'T): 'T =
        let f (sum: 'T) k = sum + (f1 k) * (f2 k)
        let init = (f1 0) * (f2 0) in (List.fold f init [1 .. l - 1])



type Matrix<'V, 'T when 'T :> Array<'V>> private(rows: int, f: int -> 'T) =

    let data:Array<'T> = Array.Create rows f
    member this.Item with get(i: int):'T = data[i]
    member this.Row i: int -> 'V = fun j -> data[i][j]
    member this.Column j: int -> 'V = fun i -> data[i][j]
    member this.Rows: int = data.Size
    member this.Columns: int = this[0].Size

    static member Var<'N>((rows, columns): int * int) (gen: int * int -> 'N) =
        let row i = Array.Buffer(columns, fun j -> gen (i, j)) in Matrix(rows, row)

    static member Const<'N>((rows, columns): int * int) (gen: int * int -> 'N) =
        let row i = Array.Create columns (fun j -> gen (i, j)) in Matrix(rows, row)

    static member Vandermonde(order: int) (inp: Array<Fraction>) =
        let row (_: int) = Array.Buffer(inp.Size, fun _ -> Fraction(1))
        let data = Array.Create (order + 1) row
        let write (i: int) (j:int) = data[i-1][j] * inp[j]
        for i in 1 .. order do data[i].Overwrite(write i)
        Matrix(data.Size, fun i -> data[i] :> Array<Fraction>)

type MatrixConst<'T> = Matrix<'T, Array<'T>>
type MatrixVar<'T> = Matrix<'T, Array.Buffer<'T>>
