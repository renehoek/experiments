

class CumSum:

    def __init__(self):
        self._sum = 0

    def __call__(self, initial: int):
        self._sum = initial
        def wrapped(value: int):
            self._sum += value
            return self._sum
        return wrapped




