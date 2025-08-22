"""
This module provides a function to check if the brackets in a string are correctly paired.
"""

bracket_map = {")": "(", "]": "[", "}": "{"}


def is_paired(input_string):
    """
    Check if all types of brackets (curly, square, and round) in the input string are correctly paired.

    Parameters:
    input_string (str): The string to be checked for paired brackets.

    Returns:
    bool: True if all types of brackets are paired correctly, False otherwise.
    """
    stack = []
    for char in input_string:
        if char in bracket_map.values():
            stack.append(char)
        elif char in bracket_map:
            if stack == [] or bracket_map[char] != stack.pop():
                return False
    return stack == []
