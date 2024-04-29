import os
import json


class Record:
    def __init__(self, d):
        self.price = d["Price"]
        self.open = d["Open"]
        self.high = d["High"]
        self.low = d["Low"]
        self.close = d["Close"]
        self.epoch = d["Epoch"]
        self.timestamp = d["Timestamp"]


class FileRecord:
    def __init__(self, filename: str):
        print("Reading: " + filename)
        with open(filename) as json_file:
            data = json.load(json_file)
        self.title = data["Title"]
        self.folder = data["Folder"]
        self.date = data["Date"]
        self.symbol = data["Symbol"]
        self.strike = float(data["Strike"])
        self.expiry = data["Expiry"]
        self.direction = data["Direction"]
        self.filename = filename[:-5]
        self.values = list(map(lambda x: Record(x), data["Values"]))


def samples():
    result = {}
    for folder in os.walk("Temp/"):
        key = os.path.join(folder[0], "data.json")
        result[key] = FileRecord(key)
    return result
