namespace Trading.Maths

open TradingLib


module Filter =

    let private transform (length, window) (row: Vector<Fraction>) =

        let half: int = if (window % 2 = 0) then window / 2 else (window - 1) / 2
        let newLength: int = 2 * half + length

        let get (i: int, j: int): Fraction =
            let b = (j > (i + 2 * half - window)) && ((i + 2* half) >= j)
            if b then row[2 * half + i - j] else Fraction(0)

        let sum (i: int) (init: int) (f: int -> int): Fraction =
            let f (s: Fraction) (k: int) = s + Fraction(2) * get(i, f k)
            let z = get(i, init) in (List.fold f z [1 .. half])

        let gen (i: int, j: int): Fraction =

            if j = 0 then
                sum i half <| fun k -> k - 1
            else if j = length - 1 then
                sum i (newLength - half) <| fun k -> newLength - k
            else
                let f0, f1, f2 = half + j, half - j, newLength + half - j
                let g1 = if f1 >= 0 then get(i, f1) else Fraction(0)
                let g2 = if f2 < newLength then get(i, f2) else Fraction(0)
                get(i, f0) - g1 - g2

        Matrix.Const<float> (length, length) (gen >> Fraction.ToFloat)

    type private ComputeState(maxOrder: int, length: int, window: int) =

        let v = Vector.Create window (fun i -> Fraction(window - 1 - 2 * i, 2))
                |> MatrixConst<Fraction>.Vandermonde maxOrder

        let input, output: MatrixVar<Fraction> * MatrixVar<Fraction> =
            let matrix = Matrix.Var<Fraction> (maxOrder + 1, maxOrder + 1)
            let gen (i, j) = Vector.Convolve window (v.Row(i), v.Row(j))
            let identity (i, j) = if i = j then Fraction(1) else Fraction(0)
            (matrix gen, matrix identity)

        let operate row f =
            input[row].Overwrite(f input) ; output[row].Overwrite(f output)

        member this.IsZero (i, j): bool = let z = input[i][j] in z.N = 0I
        member this.IsOne (i, j): bool = let z = input[i][j] in z.N = z.D

        member this.Eliminate(source, target) =
            if not((source = target) || this.IsZero(target, source)) then
                assert this.IsOne (source,source)
                let factor = input[target][source]
                operate target <| fun m k -> m[target][k] - factor * m[source][k]
                assert this.IsZero(target, source)

        member this.Normalize(row) =
            let factor = input[row][row]
            operate row <| fun m k -> m[row][k]/factor

        member this.Matrix(order: int): MatrixConst<float> =
            let gen k = Vector.Convolve (order + 1) (output.Row(0), v.Column(k))
            Vector.Create window gen |> transform (length, window)

    type Compute(maxOrder: int, length: int, window: int) =

        let map =

            assert (length >= window)
            assert (window > maxOrder)
            assert (maxOrder >= 1)

            let state = ComputeState(maxOrder, length, window)
            state.Normalize(0)

            let matrix (order: int): MatrixConst<float> =
                for k in [0 .. order - 1] do state.Eliminate(k, order)
                state.Normalize(order)
                for k in [0 .. order - 1] do state.Eliminate(order, k)
                assert state.IsOne (order, order)
                state.Matrix(order)

            Utils.CreateDictionary([1 .. maxOrder], matrix)

        member this.Matrix(order: int) = assert (order <= maxOrder) ; map[order]

        //member this.Compute(input: int -> float): Array<float> =
        //    let gen i = (this.Matrix[i] *| input) in Array.Create(newLength, gen)
