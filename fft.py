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
