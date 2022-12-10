import random
from typing import List, Callable, TypeVar
from .interface import ItemGetter

T = TypeVar("T")
V = TypeVar("V")


def quicksort(array: List[T], itemgetter: Callable[[T], V]) -> List[T]:
    # If the input array contains fewer than two elements,
    # then return it as the result of the function
    if len(array) < 2:
        return array

    low, same, high = [], [], []

    # Select your `pivot` element randomly
    pivot = itemgetter(array[random.randint(0, len(array) - 1)])

    for item in array:
        # Elements that are smaller than the `pivot` go to the `low` list. Elements that are larger than
        # `pivot` go to the `high` list. Elements that are equal to `pivot` go to the `same` list.
        value = itemgetter(item)
        if value < pivot:
            low.append(item)
        elif value == pivot:
            same.append(item)
        elif value > pivot:
            high.append(item)

    # The final result combines the sorted `low` list
    # with the `same` list and the sorted `high` list
    return quicksort(low, itemgetter) + same + quicksort(high, itemgetter)
