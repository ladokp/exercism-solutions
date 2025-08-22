"""
This module provides functions to encode and decode strings using run-length encoding.
"""

from itertools import groupby


def decode(code: str) -> str:
    """
    Decodes a run-length encoded string.

    Parameters:
    code (str): The encoded string to decode.

    Returns:
    str: The decoded string.
    """
    decoded = []
    count = ""
    for char in code:
        if char.isdigit():
            count += char
        else:
            num = int(count) if count else 1
            decoded.append(char * num)
            count = ""
    return "".join(decoded)


def encode(text: str) -> str:
    """
    Encodes a string using run-length encoding.

    Parameters:
    text (str): The string to encode.

    Returns:
    str: The run-length encoded string.
    """
    code = []
    for char, group in groupby(text):
        section_length = sum(1 for _ in group)
        if section_length > 1:
            code.append(f"{section_length}{char}")
        else:
            code.append(char)
    return "".join(code)
