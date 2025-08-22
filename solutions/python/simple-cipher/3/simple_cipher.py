"""
This module provides a simple cipher class for encoding
and decoding text using a generated or provided key.
"""

import secrets
from string import ascii_lowercase
from itertools import cycle


class Cipher:
    """
    A class used to encode and decode text using a key-based cipher.

    If no key is provided, a random key of 100 lowercase letters will be generated.
    """

    def __init__(self, key: str = None):
        """
        Initializes the Cipher with a given key or generates a random key.

        Parameters:
        key (str): A string of lowercase letters to be used as the cipher key.
                   If None, a random key will be generated.
        """
        if key is None:
            self.key = "".join(
                secrets.choice(ascii_lowercase) for _ in range(100)
            )
        else:
            self.key = key

    def encode(self, text: str, /, shifting_mode: int = 1) -> str:
        """
        Encodes the given text using the cipher key.

        Parameters:
        text (str): The text to encode.

        shifting_mode (int): Direction of the shift. Default is 1 for encoding,
                             -1 should be used for decoding.

        Returns:
        str: The encoded text.
        """
        encoded = []
        for character_1, character_2 in zip(text, cycle(self.key)):
            shift_amount = (ord(character_2) - ord("a")) * shifting_mode
            new_char_index = (ord(character_1) - ord("a") + shift_amount) % 26
            encoded.append(ascii_lowercase[new_char_index])
        return "".join(encoded)

    def decode(self, text: str) -> str:
        """
        Decodes the given text using the cipher key.

        Parameters:
        text (str): The text to decode.

        Returns:
        str: The decoded text.
        """
        return self.encode(text, shifting_mode=-1)
