# This is a sample Python script.

# Press ⇧F10 to execute it or replace it with your code.
# Press Double ⇧ to search everywhere for classes, files, tool windows, actions, and settings.

from myproperty import chatty_property
from directory import DirectorySize, DirectoryConfig

from stats import permutations, moving_avg, moving_avg2
from curry import CumSum

class Robot:
    def __init__(self, city):
        self.city = city

    @chatty_property
    def city(self):
        print("The Property 'city' will be returned now:")
        return self.__city

    @city.setter
    def city(self, city):
        print("'city' will be set")
        self.__city = city


class Storage(DirectoryConfig):
    size = DirectorySize()

    def __init__(self, basedir: str):
        self.basedir = basedir

    def getBaseDir(self) -> str:
        return self.basedir


def main():
    # r = Robot("Hendrik Ido Ambacht")
    # print("City = {c}".format(c=r.city))

    # s = Storage("/Users/renevanhoek/Downloads")
    # print("Size is: {s}".format(s=s.size))

    # for ch in permutations([1, 2, 3]):
    #     print(ch)


    # tp = [5, 10, 20, 30, 40, 50, 70]
    # for x in moving_avg(tp, 3):
    #     print(x)

    # it = moving_avg2()
    # it.send(None)
    #
    # tp = [5, 10, 20, 30, 40, 50, 70]
    # for x in tp:
    #     print("main: Send x = {x}".format(x=x))
    #     v = it.send(x)
    #     print(v)

    x = [5, 23, 6, 8, 34]
    #cumSum = CumSum()

    x2 = map(CumSum()(0), x)
    print(list(x2))






# Press the green button in the gutter to run the script.
if __name__ == '__main__':
    main()

# See PyCharm help at https://www.jetbrains.com/help/pycharm/
