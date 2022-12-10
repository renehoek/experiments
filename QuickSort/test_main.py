from unittest import TestCase
from quicksort import quicksort


class Test(TestCase):
    def setup(self):
        pass


class QuickSort(Test):
    def test_empty(self):
        b = []
        b_sorted = quicksort(b)
        self.assertIs(b_sorted, b)
        self.assertListEqual(b_sorted, [])

    def test_one_or_two(self):
        b = [4]
        b_sorted = quicksort(b)
        self.assertIs(b_sorted, b)
        self.assertListEqual(b_sorted, [4])

        b = [4, 5]
        b_sorted = quicksort(b)
        self.assertIsNot(b_sorted, b)
        self.assertListEqual(b_sorted, [4, 5])
        self.assertListEqual(b, [4, 5])

        b = [5, 4]
        b_sorted = quicksort(b)
        self.assertIsNot(b_sorted, b)
        self.assertListEqual(b_sorted, [4, 5])
        self.assertListEqual(b, [5, 4])

        b = [4, 4]
        b_sorted = quicksort(b)
        self.assertIsNot(b_sorted, b)
        self.assertListEqual(b_sorted, [4, 4])

    def test_big_list(self):
        b = [4, 5, 12, 3, 2, 2, 0, 9, 10, 12, 1000]
        b_sorted = quicksort(b)
        self.assertIsNot(b_sorted, b)
        self.assertListEqual(b_sorted, [0, 2, 2, 3, 4, 5, 9, 10, 12, 12, 1000])

        b = [-4, -5, 12, -3, -2, 2, 0, 9, 10, 12, 1000]
        b_sorted = quicksort(b)
        self.assertIsNot(b_sorted, b)
        self.assertListEqual(b_sorted, [-5, -4, -3, -2, 0, 2, 9, 10, 12, 12, 1000])

    def test_invalid_types(self):
        b = [4, 5, 12, 'a', 'b', 10]
        with self.assertRaises(TypeError):
            quicksort(b)

        self.assertListEqual(b, [4, 5, 12, 'a', 'b', 10])
