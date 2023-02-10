# This is a sample Python script.

# Press ⇧F10 to execute it or replace it with your code.
# Press Double ⇧ to search everywhere for classes, files, tool windows, actions, and settings.

class Part:
    def __init__(self):
        self.name = ""


class Element(Part):
    def __init__(self):
        super().__init__()
        self.level = 3


class Component(Part):
    def __init__(self):
        super().__init__()
        self.cost = 500
        self.the_parts = []


def main():
    e1 = Element()
    e1.name = "poot 1"

    e3 = Element()
    e3.name = "poot 2"

    e2 = Element()
    e2.name = "tafelblad"

    c = Component()
    c.name = "tafel"
    c.the_parts.append(e1)
    c.the_parts.append(e3)
    c.the_parts.append(e2)

    c1 = Component()
    c1.name = "stoel"

    c3 = Component()
    c3.name = "Eetkamer"
    c3.the_parts.append(c)
    c3.the_parts.append(c1)

# Press the green button in the gutter to run the script.
if __name__ == '__main__':
    main()

# See PyCharm help at https://www.jetbrains.com/help/pycharm/
