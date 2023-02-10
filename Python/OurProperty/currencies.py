class Ccy:
    def __init__(self, amount: float, cu: str):
        self.__amount = amount
        self.__cu = cu

    def __add__(self, other):
        if not isinstance(other, self.__class__):
            raise TypeError('Can\'t add {f}'.format(f=other))
        if self.__cu != other.__cu:
            raise TypeError('Can\'t add different currency-units')

        self.__amount += other.__amount

    @property
    def amount(self):
        return self.__amount


