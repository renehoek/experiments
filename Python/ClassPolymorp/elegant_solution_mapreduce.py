from threading import Thread
from typing import Iterator, TypeVar, List, Type
import os
import requests

T = TypeVar('T')
W = TypeVar('W')
R = TypeVar('R')


class InputData:
    """
    Abstract class representing the input-data
    """

    def read(self):
        """
        Returns the input data
        """
        raise NotImplementedError

    @classmethod
    def generate_inputs(cls, config: dict) -> Iterator[T]:
        raise NotImplementedError


class Worker:
    """
    Abstract class for the map-reduce work
    """

    def __init__(self, input_data):
        self.input_data = input_data
        self.result = None

    def map(self):
        raise NotImplementedError

    def reduce(self, other):
        raise NotImplementedError

    @classmethod
    def create_workers(cls, inputdata_class: Type[InputData], config: dict) -> List[W]:
        workers = []
        input_datas = inputdata_class.generate_inputs(config)
        for inputdata_class in input_datas:
            workers.append(cls(inputdata_class))

        return workers

    @classmethod
    def execute(cls, workers: List[W]) -> R:
        threads = [Thread(target=w.map) for w in workers]
        for thr in threads:
            thr.start()
        for thr in threads:
            thr.join()

        # noinspection PyCompatibility
        first, *rest = workers
        for worker in rest:
            first.reduce(worker)

        return first.result


# noinspection PyCompatibility
class PathInputData(InputData):
    """
    Concrete class read data from disk.
    """

    def __init__(self, path):
        super().__init__()
        self.path = path

    def read(self):
        with open(self.path) as f:
            return f.read()

    @classmethod
    def generate_inputs(cls, config: dict) -> Iterator[T]:
        for fn in os.listdir(config['datadir']):
            yield cls(os.path.join(config['datadir'], fn))


class WebsiteInputData(InputData):
    """
    Concreate class read html source from website
    """

    def __init__(self, url: str):
        super().__init__()
        self.url = url

    def read(self):
        r = requests.get(self.url)
        return r.text

    @classmethod
    def generate_inputs(cls, config: dict) -> Iterator[T]:
        for url in config['urls']:
            yield cls(url)


# noinspection PyCompatibility
class LineCounterWorker(Worker):
    """
    Count the lines in the input.
    """

    def __init__(self, input_data):
        super().__init__(input_data)
        self.input_data = input_data

    def map(self):
        self.result = self.input_data.read().count("\n")

    def reduce(self, other: Worker):
        self.result += other.result


# noinspection PyCompatibility
class SpaceCounterWorker(Worker):
    """
    Count the spaces in the input.
    """

    def __init__(self, input_data):
        super().__init__(input_data)
        self.input_data = input_data

    def map(self):
        self.result = self.input_data.read().count(" ")

    def reduce(self, other: Worker):
        self.result += other.result
