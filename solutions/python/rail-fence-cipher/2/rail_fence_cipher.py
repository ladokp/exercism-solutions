"""
This module provides functions to encode and decode text using a rail fence cipher.

Functions:
- rail_pattern(number): Generates a repeating pattern for rail positions.
- encode(plaintext, rails): Encodes the given plaintext using the specified number of rails.
- decode(ciphertext, rails): Decodes the given ciphertext using the specified number of rails.
"""

from itertools import cycle


def rail_pattern(number):
    """
    Generates a repeating pattern for rail positions based on the number of rails.

    Args:
    - number (int): The number of rails.

    Returns:
    - An iterator that cycles through the rail positions in a zigzag pattern.
    """
    rails = list(range(number))
    return cycle(rails + rails[-2:0:-1])


def encode(plaintext, rails):
    """
    Encodes the given plaintext using the specified number of rails.

    Args:
    - plaintext (str): The text to be encoded.
    - rails (int): The number of rails to use in the encoding process.

    Returns:
    - A string representing the encoded text.
    """
    pattern = rail_pattern(rails)
    return "".join(sorted(plaintext, key=lambda _: next(pattern)))


def decode(ciphertext, rails):
    """
    Decodes the given ciphertext using the specified number of rails.

    Args:
    - ciphertext (str): The text to be decoded.
    - rails (int): The number of rails used in the encoding process.

    Returns:
    - A string representing the decoded text.
    """
    pattern = rail_pattern(rails)
    indexes = sorted(range(len(ciphertext)), key=lambda _: next(pattern))
    result = [""] * len(ciphertext)
    for index, character in zip(indexes, ciphertext):
        result[index] = character
    return "".join(result)
