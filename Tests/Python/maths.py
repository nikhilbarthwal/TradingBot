from fractions import Fraction
from typing import List, Tuple, Dict, Callable, Optional
from matrix import Matrix


def original(length: int, window: int, order: int) -> Matrix:

    half = int(window / 2)
    assert length >= window
    assert window > order
    assert order >= 1

    def factor(inp: List[Fraction]) -> Matrix:
        v: Matrix = Matrix.vandermonde(order, inp)
        vt: Matrix = v.transpose()
        z: Optional[Matrix] = Matrix.invert(vt * v)
        assert z is not None
        return z * vt

    def shuffle_index(j: int) -> int:
        if j < 0:
            return -1 * j - 1

        if j >= length:
            return 2 * length - 1 - j

        return j

    def shuffle() -> Dict[Tuple[int, int], int]:
        m: Dict[Tuple[int, int], int] = {}
        for index in range(half, length - half):
            for k in range(window):
                m[(index, shuffle_index(index + half - k))] = k
        return m

    def convolve() -> Matrix:  # Tuple[Matrix, Matrix]:
        inp: List[Fraction] = \
            [Fraction(window - 1 - 2 * i, 2) for i in range(window)]
        f: Matrix = factor(inp)
        assert (f.rows == (order + 1)) and (f.columns == window)
        m: Dict[Tuple[int, int], int] = shuffle()

        def gen(row: int) -> Callable[[int, int], Fraction]:
            return lambda i, j: f[(row, m[(i, j)])] if (i, j) in m else Fraction(0)

        return Matrix(length, length, gen(0))
        # derivative = Matrix(length, length, gen(1)))

    def edge_fit() -> Matrix:

        inp = [Fraction(x) for x in range(window)]
        poly_factor: Matrix = factor(inp)
        # derivative: Matrix = Matrix(order, order + 1, lambda i, j:
        #                            Fraction(j) if (j == i + 1) else Fraction(0))

        return Matrix.vandermonde(order, inp) * poly_factor
        # derivative = Matrix.vandermonde(order - 1, inp) * derivative * poly_factor)

    def combine(cc: Matrix, ee: Matrix, i: int, j: int) -> Fraction:
        if i < half:
            return ee[(i, j)] if (j < window) else Fraction(0)
        if i >= (length - half):
            ii = i - (length - half) + (window - half)
            return Fraction(0) if (j < length - window) \
                else ee[(ii, j - length + window)]
        return cc[(i, j)]

    c = convolve()
    e = edge_fit()

    assert (c.rows == length) and (c.columns == length)
    assert (e.rows == window) and (e.columns == window)

    m: Matrix = Matrix(length, length, lambda i, j: combine(c, e, i, j))
    # m1: Matrix = Matrix(length, length, lambda i, j: combine(c[1], e[1], i, j))

    return m


def input_transform_matrix(length: int, extra: int):

    total = 2 * extra + length

    def f(i: int, j: int) -> int:
        if i < extra:
            if j == 0:
                return 2
            return -1 if j == extra - i else 0
        if i >= length + extra:
            if j == length - 1:
                return 2
            return -1 if j == (2*length + extra - 2) - i else 0
        else:
            return 1 if i - extra == j else 0

    return Matrix(total, length, lambda i, j: Fraction(f(i, j)))


def input_transform(x: List[Fraction], extra: int):

    length = len(x)
    total = 2 * extra + length
    y = [Fraction(0)] * total
    for i in range(length):
        y[i + extra] = x[i]
    first = x[0]
    last = x[-1]
    for i in range(extra):
        y[length + extra + i] = 2 * last - x[- (i + 2)]
        y[extra - i - 1] = 2 * first - x[i + 1]
    return y


def output_transform(m: Matrix, i: int, j: int, length: int, extra: int):

    total = 2 * extra + length
    assert (m.rows == total)
    assert (m.columns == total)

    def get(index: int):
        return m[i + extra, index]

    if j == 0:
        s = get(extra)
        for k in range(extra):
            s += 2 * get(k)
        return s

    if j == length - 1:
        s = get(total - 1 - extra)
        for k in range(extra):
            s += 2 * get(total - 1 - k)
        return s

    (f0, f1, f2) = extra + j, extra - j, 2 * length + extra - 2 - j
    (g1, g2) = get(f1) if f1 >= 0 else 0, get(f2) if f2 < total else 0
    g = get(f0) - g1 - g2
    return g


def compute(length: int, window: int, order: int) -> Matrix:

    half = int(window / 2)
    extra = half
    total = 2 * extra + length

    m = original(total, window, order)
    assert (m.rows == total)
    assert (m.columns == total)
    # mm = Matrix(length, extra, lambda i, j: m[i + half, j])
    # m1 = mm * input_transform(length, window)
    m2 = Matrix(length, length,
                lambda i, j: output_transform(m, i,  j, length, extra))
    # assert (m1.rows == m2.rows) and (m1.columns == m2.columns)
    return m2


# test = [Fraction(x+1) for x in range(10)]
# print([int(x) for x in input_transform(test, 3)])
# m= input_transform_matrix(len(test), 3)
# print([int(x) for x in m.multiply(test)])
