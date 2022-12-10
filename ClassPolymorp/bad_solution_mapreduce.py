class InputData:
    """
    Abstract class representing the input-data
    """

    def read(self):
        """
        Returns the input data
        """
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
