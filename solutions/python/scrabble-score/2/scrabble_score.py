"""
Module to calculate the score of a word based on predefined scores for each letter.
"""

SCORES = {
    "A": 1,
    "B": 3,
    "C": 3,
    "D": 2,
    "E": 1,
    "F": 4,
    "G": 2,
    "H": 4,
    "I": 1,
    "J": 8,
    "K": 5,
    "L": 1,
    "M": 3,
    "N": 1,
    "O": 1,
    "P": 3,
    "Q": 10,
    "R": 1,
    "S": 1,
    "T": 1,
    "U": 1,
    "V": 4,
    "W": 4,
    "X": 8,
    "Y": 4,
    "Z": 10,
}


def score(word):
    """
    Calculate the score of a word based on predefined letter scores.

    Args:
    word (str): The word to calculate the score for.

    Returns:
    int: The total score of the word.
    """
    return sum(SCORES.get(character, 0) for character in word.upper())
