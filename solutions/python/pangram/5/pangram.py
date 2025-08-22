from string import ascii_lowercase
from typing import Set

LOWER_CASE_ASCII_SET: Set[str] = set(ascii_lowercase)


def is_pangram(sentence: str) -> bool:
    """
    Check if the given sentence is a pangram.

    A pangram is a sentence containing every letter of the alphabet at least once.

    Parameters:
    sentence (str): The sentence to check.

    Returns:
    bool: True if the sentence is a pangram, False otherwise.
    """
    return LOWER_CASE_ASCII_SET - set(sentence.lower()) == set()
