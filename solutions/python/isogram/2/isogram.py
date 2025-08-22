"""
This module provides a function to check if a given string is an isogram.

An isogram is a word or phrase without a repeating letter, however spaces and hyphens are allowed to appear multiple times.
"""

import re


def is_isogram(string):
    """
    Check if the input string is an isogram.

    An isogram is a word or phrase without a repeating letter. Spaces and hyphens are ignored.

    Parameters:
    string (str): The string to check.

    Returns:
    bool: True if the string is an isogram, False otherwise.
    """
    string = re.sub(r"[ -]", "", string.lower())
    return len(string) == len(set(string))
