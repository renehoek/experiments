from functools import reduce

def permutations(items):
    n = len(items)
    if n == 0:
        yield []
    else:
        for i in range(len(items)):
            for cc in permutations(items[:i]+items[i+1:]):
                yield [items[i]] + cc


def moving_avg(items, period):
    for r in range(len(items)):
        number_set = items[r:r+period]
        s = reduce(lambda x, y: x + y, number_set)
        yield float(s / period)

def moving_avg2():
    s = 0
    n = yield None
    c = 0

    while True:
        s += n
        c += 1
        avg = float(s / c)
        n = yield avg
