from fractions import Fraction
from typing import List, Tuple, Callable


class Matrix:

    def __init__(self, rows: int, columns: int,
                 f: Callable[[int, int], Fraction]) -> None:
        assert (rows > 0) and (columns > 0)
        self.rows = rows
        self.columns = columns

        def row(i: int) -> List[Fraction]:
            return [f(i, j) for j in range(columns)]

        self.data = [row(i) for i in range(rows)]

    def __getitem__(self, pos: Tuple[int, int]) -> Fraction:
        assert pos[0] < self.rows
        assert pos[1] < self.columns
        return self.data[pos[0]][pos[1]]

    def __setitem__(self, pos: Tuple[int, int], v: Fraction) -> None:
        assert pos[0] < self.rows
        assert pos[1] < self.columns
        self.data[pos[0]][pos[1]] = v

    def __len__(self):
        return len(self.data)

    def print(self, filename: str) -> None:
        with open(filename, "w") as f:
            f.write(str(self.rows) + " " + str(self.columns) + "\n")
            for i in range(self.rows):
                for j in range(self.columns):
                    f.write(str(i) + " " + str(j) + " " + str(self[(i, j)].numerator)
                            + " " + str(self[(i, j)].denominator) + "\n")

    def __str__(self) -> str:
        result = ""
        for i in range(self.rows):
            s: str = "["
            for j in range(self.columns):
                s += " " + str(float(self.data[i][j]))
            result += s + " ]\n"
        return result

    def __mul__(self, m):  # type: ignore
        assert self.columns == m.rows

        def gen(i: int, j: int) -> Fraction:
            s = Fraction(0)
            for k in range(m.rows):
                s += self[(i, k)] * m[(k, j)]
            return s

        return Matrix(self.rows, m.columns, gen)

    def multiply(self, m: List[Fraction]) -> List[Fraction]:
        n: int = len(m)
        assert self.columns == n

        def gen(k: int) -> Fraction:
            s = Fraction(0)
            for i in range(n):
                s += self[(k, i)] * m[i]
            return s

        return [gen(k) for k in range(self.rows)]

    def __eq__(self, m) -> bool:
        if (m.rows != self.rows) or (m.columns != self.columns):
            return False
        for i in range(self.rows):
            for j in range(self.columns):
                if self[(i, j)] != m[(i, j)]:
                    return False
        return True

    def transpose(self):
        return Matrix(self.columns, self.rows, lambda i, j: self[(j, i)])

    @staticmethod
    def invert(m):
        n: int = m.rows
        assert n == m.columns
        m0: Matrix = Matrix(n, n, lambda ii, jj: m[(jj, ii)])
        m1: Matrix = Matrix(n, n, lambda ii, jj:
                            Fraction(1) if (ii == jj) else Fraction(0))

        def subtract(mm: Matrix, target: int, source: int, factor: Fraction) -> None:
            for k in range(n):
                mm[k, target] -= factor * mm[k, source]

        def swap(ii: int, jj: int):
            for k in range(n):
                temp = m0[(ii, k)]
                m0[(ii, k)] = m0[(jj, k)]
                m0[(jj, k)] = temp

        def shuffle(ii: int):
            for jj in range(ii + 1, n):
                if m0[(jj, ii)] != 0:
                    swap(jj, ii)
                    return
            return None

        for i in range(n):
            for j in range(n):
                if (i != j) and (int(m0[(i, j)]) != 0):
                    if m0[(i, i)] == 0:
                        shuffle(i)
                    f: Fraction = m0[(i, j)] / m0[(i, i)]
                    subtract(m0, j, i, f)
                    subtract(m1, j, i, f)
                    assert int(m0[(i, j)]) == 0

        for i in range(n):
            if m0[(i, i)] == 0:
                return None
            for j in range(n):
                m1[(j, i)] = m1[(j, i)] / m0[(i, i)]

        return Matrix.transpose(m1)

    @staticmethod
    def vandermonde(order: int, inp: List[Fraction]):
        assert len(inp) > 0
        m: Matrix = Matrix(len(inp), order + 1, lambda _i, _j: Fraction(1))
        for i in range(len(inp)):
            for j in range(order):
                m[(i, j + 1)] = m[(i, j)] * inp[i]
        return m
