# This is a sample Python script.
# Press ⇧F10 to execute it or replace it with your code.
# Press Double ⇧ to search everywhere for classes, files, tool windows, actions, and settings.
from typing import Dict, TypeVar

from quicksort import quicksort, ItemGetter

K = TypeVar("K")
V = TypeVar("V")

def getter(v: Dict) -> int:
    """
    The itemgetter function which returns the value to sort on.

    :param v: The item stored in the list
    :return: The value to sort on.
    """
    return v['id']


class MyGetter(ItemGetter):
    """
    Implementation of the Interface ItemGetter
    """

    def __call__(self, v: Dict[K, V]) -> V:
        """

        :param v:
        :return:
        """
        return v['id']


def main():
    the_list = [{'id': 5, 'name': 'Frank'},
                {'id': 4, 'name': 'Karel'},
                {'id': 9, 'name': 'Marcel'},
                {'id': 8, 'name': 'Madelon'},
                {'id': 2, 'name': 'Loes'}]

    # the_list = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
    # the_list = [0, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]
    my_getter = MyGetter()
    the_list_sorted = quicksort(the_list, my_getter)

    print(the_list_sorted)


# Press the green button in the gutter to run the script.
if __name__ == '__main__':
    main()

# See PyCharm help at https://www.jetbrains.com/help/pycharm/
