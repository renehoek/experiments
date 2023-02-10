from typing import List, Optional


def quicksort(the_list: List) -> List:
    return _quicksort(the_list, 0, len(the_list) - 1)


def _quicksort(the_list: List, low: int, high: int) -> List:
    """
    Quicksort the list in-place.
    :param the_list: The list to be sorted.
    :param low: Start-index
    :param high: End-index
    :return:
    """
    if low >= high or low < 0:
        return the_list

    p = _partition(the_list, low, high)
    _quicksort(the_list, low, p - 1)
    _quicksort(the_list, p + 1, high)

    return the_list


def _partition(the_list: List, low: int, high: int) -> Optional[int]:
    """
    Returns the pivot-index to use to divide the list
    :param the_list: The list
    :param low: The start-index
    :param high: The end-index
    :return: The index of the pivot to use
    """

    new_pivot_idx = low - 1
    pivot = the_list[high]

    for idx in range(low, high):
        if the_list[idx] <= pivot:
            new_pivot_idx += 1
            _swap(the_list, idx, new_pivot_idx)

    new_pivot_idx += 1
    _swap(the_list, new_pivot_idx, high)

    return new_pivot_idx


def _swap(the_list, idx1, idx2):
    # print("Swap: {v1} at {idx1} with {v2} at {idx2} The list: {t}".format(v1=the_list[idx1], idx1=idx1,
    #                                                                      v2=the_list[idx2], idx2=idx2, t=the_list))
    v_ = the_list[idx1]
    the_list[idx1] = the_list[idx2]
    the_list[idx2] = v_
