# This is a sample Python script.

# Press ⇧F10 to execute it or replace it with your code.
# Press Double ⇧ to search everywhere for classes, files, tool windows, actions, and settings.

def addRow(source: list, lastvalue: int, target: list):
    if len(source) == 0:
        target.append(lastvalue)
        return

    head = source[0]
    tail = source[1:]
    newvalue = lastvalue + head
    target.append(newvalue)
    addRow(tail, head, target)


def triangle(source: list, count: int, limit: int):
    if count >= limit:
        return

    lastRow = source[-1]
    lastValue = lastRow[0]
    target = []
    addRow(lastRow, lastValue, target)
    source.append(target)
    triangle(source, count + 1, limit)

def main():
    the_list = [[0, 1, 0]]
    triangle(the_list, 0, 5)
    print("{t}".format(t=the_list))


# Press the green button in the gutter to run the script.
if __name__ == '__main__':
    main()

# See PyCharm help at https://www.jetbrains.com/help/pycharm/
