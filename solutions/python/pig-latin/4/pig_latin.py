"""
This module provides a function to translate English sentences into Pig Latin.

The main function, `translate`, takes an input string and transforms each word
into its Pig Latin equivalent by following specific rules for consonants and
vowels. It's useful for fun linguistic exercises or as a playful conversion tool.
"""

from re import sub


def translate(text: str) -> str:
    """Convert a sentence into Pig Latin.

    This function takes a text input and translates each word into Pig Latin.
    The first consonant or consonant cluster is moved to the end of the word,
    and 'ay' is appended. If a word begins with a vowel sound, 'ay' is simply
    added to the end.

    Args:
        text (str): The input string containing words to be translated.

    Returns:
        str: A new string where each word has been transformed into Pig Latin.
    """
    return " ".join(
        sub(
            r"([xy](?=[aeiouy])|s*qu|[bcdfghjklmnpqrstvwz]*)(.*)",
            r"\2\1",
            word,
        )
        + "ay"
        for word in text.split()
    )
