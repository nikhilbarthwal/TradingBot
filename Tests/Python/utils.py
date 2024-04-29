from typing import List
import numpy
import multiprocessing
import json
from fractions import Fraction

precision = 5
precision_limit = float(Fraction(10, 10 * precision))


def diff(a: float, b: float) -> float:
    if abs(a-b) < precision_limit:
        return 0.0
    return abs(round(abs(b - a) * 100 / a, 3))


def gen_data(length: int):
    x = numpy.linspace(0, 2 * numpy.pi, length)
    y = 1.5 + numpy.sin(x) + numpy.random.random(length) * 0.2
    return [round(float(i), precision) for i in y]


def parallel_map(f, inp: List[int]):
    pool_size = multiprocessing.cpu_count()
    pool = multiprocessing.Pool(pool_size)
    results = pool.map(f, inp)
    pool.close()
    pool.join()
    return results


def read_json(filename: str):
    with open(filename) as json_file:
        return json.load(json_file)
