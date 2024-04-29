import glob
import utils
from filter import Filter
import logging
import random

logging.basicConfig(format='%(levelname)s %(asctime)s : %(message)s',
                    level=logging.INFO)


def main():
    max_error: float = 0.0
    for filename in glob.glob("FilterTest/Temp/*.json"):
        data = utils.read_json(filename)
        window: int = data["window"]
        length: int = data["length"]
        order: int = data["order"]
        f: Filter = Filter(window, order, length)
        e = f.test(data)
        logging.info(f"DATA Filename: {filename} / Window: {window} / Length: " +
                     f"{length} / Order: {order} / Error: {e}")
        if e > max_error:
            max_error = e
    logging.info(f"Max Error: {max_error}")


def compute_test():
    for order in range(7, 1, -1):
        for n in range(10):
            length: int = random.randint(15, 120)
            window: int = random.randint(10, length)
            data = utils.gen_data(length)
            logging.info(
                f"Compute -> Window: {window} / Order: {order} / Length: {length}")
            f: Filter = Filter(window, order, length)
            _ = f.compute(data)


if __name__ == '__main__':
    logging.info(" *** START ***")
    main()
    compute_test()
    logging.info(" *** THE END ***")
