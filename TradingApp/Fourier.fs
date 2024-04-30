module TradingApp

open TradingLib

module Fourier =
    
    type private Fourier<'T> private(factor: float, dir: float, l: Vector<int>,
            init: Vector<int> -> Vector<'T> -> Vector.Buffer<Complex> -> bool) =
        do assert (l.Size % 2 = 0)
        let half = l.Size / 2
        let zeroes = Vector.Create half (fun _ -> Complex(0.0, 0.0))
        let factors: Vector<Complex> =
            let phase(i: int) = dir * Complex.Pi * (float i) / (float l.Size)
            Vector.Create l.Size (phase >> Complex)

        let child (offset: int) =
            let h = Vector.Create half (fun i -> l[2 * i + offset])
            Fourier(factor, dir, h, init)

        let index (even: Vector<Complex>, odd: Vector<Complex>) (i: int): Complex =
            if i < half then (even[i] + factors[i] * odd[i]) / factor
                        else (even[i - half] + factors[i] * odd[i - half]) / factor

        let combine (output: Vector.Buffer<Complex>) (even, odd) =
            output.Overwrite(index (even, odd))

        interface Node<'T, Complex> with
            member this.Size = l.Size
            member this.Split() =
                assert (l.Size > 1)
                { Left = (child 0) ; Right = (child 1) }

            member this.Init(input: Vector<'T>, output): bool =
                assert ((input.Size = 1) && (output.Size = 1))
                init l input output

            member this.Combine(_, output, even, odd): bool =
                match even, odd with
                | Yes(e), Yes(o) -> combine output (e, o) ; true
                | Yes(e), No -> combine output (e, zeroes) ; true
                | No, Yes(o) -> combine output (zeroes, o) ; true
                | No, No -> false
        
        static member Create(factor, dir, size: int, init): Tree<'T, Complex> =
            let l = Vector.Create size <| fun i -> i + 1
            let node: Node<'T, Complex> = Fourier(factor, dir, l, init)
            Tree(node, (fun _ -> Complex(0.0, 0.0)))

    type private FFT(size: int) =
        let init (l: Vector<int>) (input: Vector<float>)
                 (output: Vector.Buffer<Complex>) =
            output.Overwrite(fun _ -> Complex(input[l[0]], 0.0)) ; true
        let fourier = Fourier<float>.Create(1.0, -1.0, size, init)
        member this.Eval(input: Vector<float>) = fourier.Eval(input)
        member this.Data: Vector<Complex> = fourier

    type private InverseFFT(size: int) =
        let init (l: Vector<int>) (input: Vector<Complex>)
                 (output: Vector.Buffer<Complex>) =
            let c: Complex = input[l[0]]
            output.Overwrite(fun _ -> c) ; not <| c.Zero()
        let fourier = Fourier<Complex>.Create(2.0, 1.0, size, init)
        member this.Eval(input: Vector<Complex>) = fourier.Eval(input)
        interface Vector<float> with
            member this.Size = size
            member this.Item with get(k: int): float = fourier.Data[k].Real

(*
type Node<'Input, 'Output> =
    abstract Size: int
    abstract Combine: Vector<'Input> * Vector.Buffer<'Output> *
                      Maybe<Vector<'Output>> * Maybe<Vector<'Output>> -> bool
    abstract Split: unit -> Pair<Node<'Input, 'Output>, Node<'Input, 'Output>>
    abstract Init: Vector<'Input> * Vector.Buffer<'Output> -> bool
    
    type Merge
    type Node<int, 'R> =
    abstract Size: int
    abstract Combine: 'T * 'R * 'R -> 'R
    abstract Split: unit -> Pair<Node<'T, 'R>>
    abstract Init: 'T -> 'R


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
