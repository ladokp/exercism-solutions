"""
This module provides a function to generate a proverb based on
a list of words and an optional qualifier.
"""


def proverb(*words, qualifier=None):
    """
    Generate a proverb based on a list of words and an optional qualifier.

    Args:
        *words: A list of words to construct the proverb.
        qualifier (str, optional): An optional qualifier to modify the first word in the final line.

    Returns:
        list of str: A list of strings representing lines of the constructed proverb.
    """
    if not words:
        return []

    lines = [
        f"For want of a {words[index]} the {words[index + 1]} was lost."
        for index in range(len(words) - 1)
    ]
    lines.append(
        f"And all for the want of a {qualifier + ' ' if qualifier else ''}{words[0]}."
    )
    return lines
