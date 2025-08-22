"""
This module provides a function to perform Caesar cipher rotation on text.
"""

import string


def rotate(text, key):
    """
    Rotate the characters in the given text by the specified key using a Caesar cipher.

    Parameters:
    text (str): The string to be rotated.
    key (int): The number of positions each letter in the text is shifted.

    Returns:
    str: The rotated string with each letter shifted by the given key.
    """
    def shift_char(character, key_, alphabet):
        """
        Shifts a character by a given key within a specified alphabet.
        Parameters:
        character (str): The character to be shifted.
        key_ (int): The shift key indicating how many positions to move the
                    character in the alphabet.
        alphabet (str): The string of characters representing the alphabet.

        Returns:
        str: The shifted character if it is in the alphabet, otherwise returns
             the original character.
        """
        if character in alphabet:
            return alphabet[(alphabet.index(character) + key_) % len(alphabet)]
        return character

    rotated_text = []
    for char in text:
        if char.isupper():
            rotated_text.append(shift_char(char, key, string.ascii_uppercase))
        elif char.islower():
            rotated_text.append(shift_char(char, key, string.ascii_lowercase))
        else:
            rotated_text.append(char)

    return "".join(rotated_text)
