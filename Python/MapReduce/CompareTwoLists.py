import functools

def what_does_map_do():
    l1 = [10, 20, 30, 40, 50]
    l2 = [20, 30, 50, 40, 70]
    l3 = [10, 20, 30, 40, 50]

    # l will be: [False, False, False, True, False]
    l = list(map(lambda p, q: p == q, l1, l2))

    pass

def main():
    l1 = [10, 20, 30, 40, 50]
    l2 = [20, 30, 50, 40, 70]
    l3 = [10, 20, 30, 40, 50]

    if functools.reduce(lambda x, y : x and y, map(lambda p, q: p == q,l1,l2), True):
        print ("The lists l1 and l2 are the same")
    else:
        print ("The lists l1 and l2 are not the same")

    if functools.reduce(lambda x, y : x and y, map(lambda p, q: p == q,l1,l3), True):
        print ("The lists l1 and l3 are the same")
    else:
        print ("The lists l1 and l3 are not the same")

if __name__ == "__main__":
    #main()
    what_does_map_do()