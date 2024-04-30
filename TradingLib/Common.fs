namespace TradingLib


[<Struct>] type Maybe<'T> = Yes of 'T | No

[<Struct>] type Pair<'Left, 'Right> = { Left: 'Left ; Right: 'Right }


type Vector<'T> =
    abstract Size: int
    abstract Item : int -> 'T with get

module Vector =

    type Buffer<'T> (size: int, f: int -> 'T) =
        let data: 'T[] = [| for i in 1 .. size -> f (i - 1) |]
        member this.Item with get(i: int) = data[i]
        member this.Size: int = size

        member this.Overwrite(f: int -> 'T): unit =
            for i in 0 .. (size-1) do data[i] <- (f i)

        interface Vector<'T> with
            member this.Item with get(i: int) = data[i]
            member this.Size = size

    let Create<'T>(size: int) (f: int -> 'T): Vector<'T> = Buffer(size, f)

    let inline Convolve<'T when ^T: (static member (*) : 'T * 'T -> 'T)
                            and ^T: (static member (+) : 'T * 'T -> 'T)>
            (l: int) (f1: int -> 'T, f2: int -> 'T): 'T =
        let f (sum: 'T) (k: int): 'T =  sum + (f1 k) * (f2 k)
        let init = (f1 0) * (f2 0) in (List.fold f init [1 .. l - 1])

    type Circular<'T>(size: int, f: int -> 'T) =
        let mutable pos: int = 0
        let mutable count: int = 0
        let data = [| for i in 1 .. size -> f(i - 1) |]
        let get (index: int) = data[(size + pos - index - 1) % size]

        member this.Insert(x: 'T) = data[pos] <- x ; count <- count + 1
                                    pos <- pos + 1 ; if pos = size then pos <- 0

        member this.Reset() = count <- 0 ; pos <- 0

        member this.Get(buffer: Buffer<'T>): bool =
            if count < size then false else (buffer.Overwrite(get) ; true)

type Matrix<'V, 'T when 'T :> Vector<'V>> private(rows: int, f: int -> 'T) =

    let data:Vector<'T> = Vector.Create rows f
    member this.Item with get(i: int):'T = data[i]
    member this.Row i: int -> 'V = fun j -> data[i][j]
    member this.Column j: int -> 'V = fun i -> data[i][j]
    member this.Rows: int = data.Size
    member this.Columns: int = this[0].Size

    static member Var<'N>((rows, columns): int * int) (gen: int * int -> 'N) =
        let row i = Vector.Buffer(columns, fun j -> gen (i, j)) in Matrix(rows, row)

    static member Const<'N>((rows, columns): int * int) (gen: int * int -> 'N) =
        let row i = Vector.Create columns (fun j -> gen (i, j)) in Matrix(rows, row)

    static member Vandermonde(order: int) (inp: Vector<Fraction>) =
        let row (_: int) = Vector.Buffer(inp.Size, fun _ -> Fraction(1))
        let data = Vector.Create (order + 1) row
        let write (i: int) (j:int) = data[i-1][j] * inp[j]
        for i in 1 .. order do data[i].Overwrite(write i)
        Matrix(data.Size, fun i -> data[i] :> Vector<Fraction>)

type MatrixConst<'T> = Matrix<'T, Vector<'T>>
type MatrixVar<'T> = Matrix<'T, Vector.Buffer<'T>>


type Node<'Input, 'Output> =
    abstract Size: int
    abstract Combine: Vector<'Input> * Vector.Buffer<'Output> *
                      Maybe<Vector<'Output>> * Maybe<Vector<'Output>> -> bool
    abstract Split: unit -> Pair<Node<'Input, 'Output>, Node<'Input, 'Output>>
    abstract Init: Vector<'Input> * Vector.Buffer<'Output> -> bool

type Tree<'Input, 'Output>(z: Node<'Input, 'Output>, dummy: unit -> 'Output) =
    let data = Vector.Buffer(z.Size, fun _ -> dummy())
    let subTree =
        if z.Size = 1 then No else
            assert (z.Size % 2 = 0)
            let half = z.Size / 2
            let sub = z.Split()
            assert ((sub.Left.Size = half) && (sub.Right.Size = half))
            Yes({Left = Tree(sub.Left, dummy) ; Right = Tree(sub.Right, dummy)})

    member this.Data: Vector<'Output> = data
    member this.Eval(x: Vector<'Input>): bool =
        let f (t: Tree<'Input, 'Output>) = if t.Eval(x) then Yes(t.Data) else No
        match subTree with
        | No -> z.Init(x, data)
        | Yes(sub) -> z.Combine(x, data, f(sub.Left), f(sub.Right))    

    interface Vector<'Output> with
        member this.Item with get(i: int) = data[i]
        member this.Size = z.Size
