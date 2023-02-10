"""
Defines interfaces for the quicksort function
"""
import abc
from abc import abstractmethod
from typing import TypeVar

T = TypeVar("T")
V = TypeVar("V", str, int)


# noinspection PyCompatibility
class ItemGetter(metaclass=abc.ABCMeta):
    """
    The interface to fetch a key for comparison
    """

    @abstractmethod
    def __call__(self, t: T) -> V:
        """

        :param t: The value in the collection
        :return: The key for comparison
        """
        raise NotImplementedError("Implement the get method")
