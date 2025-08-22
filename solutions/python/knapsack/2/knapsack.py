"""
This module provides a function to calculate the maximum value that can be
obtained with a given maximum weight constraint and a list of items.

Each item is represented as a dictionary with 'weight' and 'value' keys.
"""


def maximum_value(maximum_weight, items):
    """
    Calculate the maximum value that can be obtained without exceeding the maximum weight.

    Args:
        maximum_weight (int): The maximum allowable weight.
        items (list): A list of dictionaries, each containing 'weight' and 'value' keys.

    Returns:
        int: The maximum total value of the selected items within the weight constraint.
    """
    combinations = [(0, 0)]
    for item in items:
        combinations += [
            (weight + item["weight"], value + item["value"])
            for weight, value in combinations
        ]

    return max(
        value for weight, value in combinations if weight <= maximum_weight
    )
