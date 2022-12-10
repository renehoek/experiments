# This is a sample Python script.

# Press ⇧F10 to execute it or replace it with your code.
# Press Double ⇧ to search everywhere for classes, files, tool windows, actions, and settings.

from fraction import Fraction

def Person_init(self, name):
    self.name = name

def Person_sayHello(self) -> str:
    return "Hi, I am {n}".format(n=self.name)

def main():

    Person = type("Person", (), {'name': 'Rene', "__init__": Person_init, "sayHello": Person_sayHello})
    p = Person("Frank")
    print("{p}".format(p=p.sayHello()))

    print(Fraction(3, 30))

    fr = Fraction(10, 50)
    fr.gcd(10, 20)
    print("Class method call: {f}".format(f=fr.reduce(5, 10)))
    print("Class method call: {f}".format(f=Fraction.reduce(5, 10)))


# Press the green button in the gutter to run the script.
if __name__ == '__main__':
    main()

# See PyCharm help at https://www.jetbrains.com/help/pycharm/
