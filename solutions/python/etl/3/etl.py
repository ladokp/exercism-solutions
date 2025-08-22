"""
This module provides functionality to transform legacy data into a new format.
The transformation involves converting list elements into dictionary keys.

Functions:
    - transform: Transforms the input dictionary by making list elements the keys.
"""


def transform(legacy_data):
    """
    Transforms legacy data where each key in the input dictionary
    corresponds to a list of values. The returned dictionary will
    have each value from the lists as keys (converted to lowercase),
    and their corresponding former key as their value.

    Args:
        legacy_data (dict): A dictionary with keys mapping to lists of values.

    Returns:
        dict: A new dictionary with values as keys (in lowercase) and
              the original keys as values.
    """
    return {
        value.lower(): key
        for key, values in legacy_data.items()
        for value in values
    }
