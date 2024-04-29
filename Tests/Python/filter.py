from typing import List
from fractions import Fraction
from matrix import Matrix
import maths
from scipy.signal import savgol_filter
import utils

max_error = 0.01


def convert(data: List[Fraction]):
    return [float(x) for x in data]


def get_errors(a: List[float], b: List[float]) -> List[float]:
    assert len(b) == len(a)
    return [abs(utils.diff(a[i], b[i])) for i in range(len(b))]


class Filter:

    def __init__(self, window: int, order: int, length: int) -> None:
        self.window = window
        self.order = order
        self.length = length
        self.half = int(window / 2)
        self.extra = self.half
        self.total = 2 * self.extra + length

        assert length >= self.window
        self.matrix: Matrix = maths.compute(self.length, self.window, self.order)
        assert (self.matrix.rows == self.length)
        assert (self.matrix.columns == self.length)

    def verify(self, data: List[Fraction], output: List[float]):
        if self.order > 6:
            return
        assert (len(data) == self.length)
        x: List[Fraction] = maths.input_transform(data, self.extra)
        assert (len(x) == self.total)
        xx: List[float] = [float(f) for f in x]
        y: List[float] = list(savgol_filter(xx, self.window, self.order))
        z: List[float] = y[self.extra:-1 * self.extra]
        errors = get_errors(output, z)
        error = max(errors)
        if error >= max_error:
            d = [(output[i], z[i]) for i in range(len(errors))
                 if errors[i] >= max_error]
            print(f"Max Error is {error} -> {d}")
        assert (error <= max_error)

    def compute(self, data: List[float]) -> List[float]:
        assert len(data) == self.length
        inp: List[Fraction] = [Fraction(d) for d in data]
        v: List[Fraction] = self.matrix.multiply(inp)
        output: List[float] = [float(x) for x in v]
        self.verify(inp, output)
        return output

    def test(self, data) -> float:
        error: float = 0.0
        assert self.window == data["window"]
        assert self.length == data["length"]
        assert self.order == data["order"]
        assert self.length == len(data["values"])
        values = data["values"]
        for i in range(self.length):
            value = values[i]
            assert len(value) == self.length
            for j in range(self.length):
                python = round(float(self.matrix[i, j]), utils.precision)
                fsharp = round(value[j], utils.precision)
                e = utils.diff(python, fsharp)
                if e > error:
                    error = e
        return error
