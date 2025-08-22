"""This module provides a function to find the index of a value in a list."""


def find(search_list: list[int], value: int) -> int:
    """Perform binary search to find the index of a value in a sorted list.

    Args:
        search_list (list[int]): The list to search in.
        value (int): The value to search for.

    Returns:
        int: The index of the value in the list.

    Raises:
        ValueError: If the value is not found in the list.
    """

    def binary_search(arr: list[int], low: int, high: int, value_: int) -> int:
        """Recursive binary search algorithm.

        Args:
            arr (list[int]): The list to search in.
            low (int): The starting index of the search range.
            high (int): The ending index of the search range.
            value_ (int): The value to search for.

        Returns:
            int: The index of the value in the list.

        Raises:
            ValueError: If the value is not found in the list.
        """
        if high >= low:
            mid = (high + low) // 2
            if arr[mid] == value_:
                return mid
            if arr[mid] > value_:
                return binary_search(arr, low, mid - 1, value_)
            return binary_search(arr, mid + 1, high, value_)
        raise ValueError("value not in array")

    return binary_search(search_list, 0, len(search_list) - 1, value)
