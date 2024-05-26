namespace TradingLib


[<Struct>] type Maybe<'T> = Yes of 'T | No

[<Struct>] type Pair<'T> = { Left: 'T ; Right: 'T }


type Vector<'T> =
    abstract Size: int
    abstract Item : int -> 'T with get

module Vector =

    type Buffer<'T> (size: int, f: int -> 'T) =
        let data: 'T[] = [| for i in 1 .. size -> f (i - 1) |]
        member this.Item with get(i: int) = data[i] and set(i: int) x = data[i] <- x
        member this.Size: int = size

        member this.Overwrite(f: int -> 'T): unit =
            for i in 0 .. (size-1) do data[i] <- (f i)

        interface Vector<'T> with
            member this.Item with get(i: int) = data[i]
            member this.Size = size

    let Create<'T>(size: int) (f: int -> 'T): Vector<'T> = Buffer(size, f)

    let Sequence (n: int) = Create n (fun i -> i + 1)

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


type Node<'T, 'R> =
    abstract Size: int
    abstract Combine: ('T * 'R * 'R) -> 'R
    abstract Split: unit -> Pair<Node<'T, 'R>>
    abstract Init: 'T -> 'R

type Tree<'T, 'R, 'N when 'N :> Node<'T, 'R>>(z: 'N) =
    let subTree = if z.Size = 1 then No else
                      assert (z.Size % 2 = 0)
                      let half = z.Size / 2
                      let sub = z.Split()
                      assert ((sub.Left.Size = half) && (sub.Right.Size = half))
                      Yes({Left = Tree(sub.Left) ; Right = Tree(sub.Right)})

    member this.Eval(x: 'T): 'R =
        match subTree with
        | No -> z.Init(x)
        | Yes(sub) -> let left, right = sub.Left.Eval(x), sub.Right.Eval(x)
                      z.Combine(x, left, right)
