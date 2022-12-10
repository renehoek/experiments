# This is a sample Python script.

# Press ⇧F10 to execute it or replace it with your code.
# Press Double ⇧ to search everywhere for classes, files, tool windows, actions, and settings.
from collections.abc import Callable
from typing import Any
import logger

Callback = Callable[..., Any]


def timed(func: Callback) -> Callback:
    """
    Time the execution of the function 'func'
    :param func:
    :return:
    """

    def func_wrapper(*args, **kwargs) -> tuple[Any, float]:
        import time
        s = time.time()
        result = func(*args, **kwargs)
        e = time.time()
        return result, e - s

    return func_wrapper


@timed
def calc_sum(numbers: list[int]) -> int:
    """
    Calculate the sum of the number in list l
    :param numbers: The numbers to sum
    :return: The sum of the numbers
    """

    s = 0
    for x in numbers:
        s += x
    return s


@timed
def calc_prod(numbers: list[int]) -> int:
    """
    Calculate the product of the numbers in the list.

    :param numbers: The list of numbers.
    :return: The product of the numbers
    """
    s = 1
    for x in numbers:
        s *= x

    return s


def main():
    # calc_sum_w = timed(calc_sum)
    # calc_prod_w = timed(calc_prod)

    the_list = [x for x in range(1, 1000000)]
    logger.log_text("Length of list: {l}".format(l=len(the_list)))

    calc_sum("A")
    the_sum, duration = calc_sum(the_list)
    logger.log_text("Sum is: {s}, it took: {t:.5f}".format(s=the_sum, t=duration))

    the_list = the_list[0:200]
    the_prod, duration = calc_prod(the_list)
    logger.log_text("Prod is: {s}, it took: {t:.5f}".format(s=the_prod, t=duration))


# Press the green button in the gutter to run the script.
if __name__ == '__main__':
    main()
