module TradingApp

(*
import numpy as np
import random
from typing import List
import math


def test(a, b):
    if len(a) != len(b):
        return False
    for i in range(len(a)):
        if round(abs(a[i] - b[i]), 5) != 0.0:
            return False
    return True


class FFT:

    def __init__(self, indexes: List[int], inverse: bool):
        self.k = 2 if inverse else 1
        self.indexes: List[int] = indexes
        self.N: int = len(indexes)
        if self.N > 1:
            assert self.N % 2 == 0
            self.even = FFT(
                [indexes[i] for i in range(self.N) if i % 2 == 0], inverse)
            self.odd = FFT(
                [indexes[i] for i in range(self.N) if i % 2 == 1], inverse)
            self.factor = []
            for i in range(self.N):
                k = 1 if inverse else -1
                theta = k * 2 * np.pi * i / self.N
                self.factor.append(math.cos(theta) + 1j * math.sin(theta))

    def compute(self, x):
        assert self.N > 0
        if self.N == 1:
            return [x[self.indexes[0]]]

        assert self.N % 2 == 0
        even = self.even.compute(x)
        odd = self.odd.compute(x)
        half = self.N // 2
        assert len(even) == half
        assert len(odd) == half
        r = [0 + 0j] * self.N
        for i in range(half):
            r[i] = (even[i] + self.factor[i] * odd[i]) / self.k
            r[half + i] = (even[i] + self.factor[half + i] * odd[i]) / self.k
        return r


def run(n):
    indexes = list(range(2**n))
    f = FFT(indexes, False)
    inv_f = FFT(indexes, True)
    x = [random.randint(0, 1000) for _ in indexes]
    y = f.compute(x)
    assert test(y, np.fft.fft(x))
    assert test(x, inv_f.compute(y))


run(10)

_________________________________________________________________________________

class FFT & IFFT

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
