"""
This module provides functions to encrypt plain text using a columnar transposition cipher.
"""

import math
import string


def cipher_text(plain_text: str) -> str:
    """Encrypts the given plain text using a columnar transposition cipher.

    Args:
        plain_text (str): The text to be encrypted.

    Returns:
        str: The encrypted cipher text.
    """

    # Convert the plain text to lowercase, strip whitespace, and remove punctuation
    text = (
        plain_text.lower()
        .strip()
        .translate(
            str.maketrans("", "", string.punctuation + string.whitespace)
        )
    )

    # Return empty text if input is empty after processing
    if not text:
        return text

    # Calculate the length of the processed text
    text_length = len(text)
    # Determine the number of columns based on the square root of the text length
    columns = math.ceil(math.sqrt(text_length))
    # Calculate the number of rows needed
    rows = math.ceil(text_length / columns)
    # Pad the text with spaces to fit into the columnar grid
    text += " " * ((columns * rows) - text_length)
    # Rearrange the text into cipher format by columns and join with spaces
    return " ".join(text[index::columns] for index in range(columns))
