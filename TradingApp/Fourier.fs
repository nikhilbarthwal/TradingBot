module TradingApp

open TradingLib

module Fourier =

    let private getFactors (offset: float) (size: int) =
        let phase(i: int) = offset * 2.0 * Complex.Pi * (float i) / (float size)
        Vector.Create size (phase >> Complex)

    let private index (half: int) (offset: float) (factors: Vector<Complex>)
                      (even: Vector<Complex>, odd: Vector<Complex>) (i: int) =
       if i < half then (even[i] + factors[i] * odd[i]) / offset
                   else (even[i - half] + factors[i] * odd[i - half]) / offset

    let private child (offset: int) (l: Vector<int>) =
        let size: int = l.Size
        assert (size % 2 = 0)
        let half: int = size / 2 in Vector.Create half (fun i -> l[2 * i + offset])

    type private FFT private(l: Vector<int>) =
        let size: int = l.Size
        do assert ((size = 1) || (size % 2 = 0))
        let half: int = size / 2
        let factors = getFactors -1.0 size
        let data = Vector.Buffer(size, fun _ -> Complex(0, 0))
        let f (z: Vector<float>) i = Complex(z[l[i]], 0.0)
        let child offset = FFT(child offset l)
        interface Node<Vector<float>, Vector<Complex>> with
            member this.Size = size
            member this.Split() =
                assert (size > 1) ; { Left = (child 0) ; Right = (child 0) }

            member this.Init(z: Vector<float>): Vector<Complex> =
                assert ((z.Size = 1) && (l.Size = 1)) ; data.Overwrite(f z) ; data

            member this.Combine(_, even, odd): Vector<Complex> =
                assert ((even.Size = half) && (odd.Size = half))
                data.Overwrite(index half 1.0 factors (even, odd)) ; data

        static member Create size = Tree(FFT <| Vector.Create size (fun i -> i))

    type private InvFFT private(l: Vector<int>) =
        let size: int = l.Size
        do assert ((size = 1) || (size % 2 = 0))
        let half: int = size / 2
        let factors = getFactors 1.0 size
        let data = Vector.Buffer(size, fun _ -> Complex(0, 0))
        let f (z: Vector<Complex>) i = z[l[i]]
        let child offset = InvFFT(child offset l)
        interface Node<Vector<Complex>, Pair<bool, Vector<Complex>>> with
            member this.Size = size
            member this.Split() =
                assert (size > 1) ; { Left = (child 0) ; Right = (child 0) }

            member this.Init(z: Vector<Complex>): Pair<bool, Vector<Complex>> =
                assert ((z.Size = 1) && (l.Size = 1)) ; data.Overwrite(f z)
                { Left = (z[l[0]].Real = 0.0) ; Right = data }

            member this.Combine(_, even, odd): Pair<bool, Vector<Complex>> =
                assert ((even.Size = half) && (odd.Size = half))
                data.Overwrite(index half 2.0 factors (even, odd)) ; data

        static member Create size = Tree(InvFFT <| Vector.Create size (fun i -> i))



    type Merge
    type Node<int, 'R> =
    abstract Size: int
    abstract Combine: 'T * 'R * 'R -> 'R
    abstract Split: unit -> Pair<Node<'T, 'R>>
    abstract Init: 'T -> 'R


(*

class MergeSort(N) # Use the same structor as FFT
        assert N % 2 = 0
        member this.Eval(f: int -> float)
        member Item with get(i) = data[i]

class Indexs(N, partitions)
        FFT(N)
        assert N % 2 = 0
        assert N % (2 * partitions) = 0
        let size = N / (2 * partitions)
        let create (_:int) = Array.Buffer(size, fun _ -> 0)
        let data = Array.Create(partitions, create)
        let MergeSort(N/2)
        let partitions = [0 .. (paritions-1)]
        let set partition index = IndexSorter.Result[row*size + column]
        let amplitude i = FFT[i].Real

        member this.Max = partitions
        member this.Set(partition, array):
            assert 0 < n < parititons
            for n in data[partition]:
                array[n] = 0
                array[N-n] = 0
        member this.Initialize(input)
                FFT.eval(input)
                IndexSorter.eval(amplitude)
                for partiion in partitions do
                    data[partition].Overwrite(set partition)

class Curve
    - IFFT: Contains both current & previous result, [] is with previous
    - current: Complex array
    - previous: Complex array
    - IndexPartition

    -rec bool FindBest(input, partition)
            if partition = IndexPatition.Max then false else
                IndexParition.Set(current, parition)
                error = InvDFT.Eval(current, partition, input)

                if error > minError then true else
                    previous += current
                    FindBest(input, partition+1)
    - let output Input i = Input[i] - IFFT.Previous[i]
    + bool Eval(Input: IVector, Output, Vector):
            current.Overwrite(zero)
            previous.Overwite(zero)
            FFT.eval(input)
            Indexs.Initialize(FFT.result)

            if FindBest(input, 0) then
                    Output.Overwrite(output Input)
                    return True
            else False
*)
