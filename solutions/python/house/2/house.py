"""
This module provides a function to recite verses of 'The House That Jack Built'.

Functions:
- recite(start_verse: int, end_verse: int) -> List[str]:
    Recites verses from the given start to the end.
"""

from typing import List

TEXT = (
    "the horse and the hound and the horn that belonged to ",
    "the farmer sowing his corn that kept ",
    "the rooster that crowed in the morn that woke ",
    "the priest all shaven and shorn that married ",
    "the man all tattered and torn that kissed ",
    "the maiden all forlorn that milked ",
    "the cow with the crumpled horn that tossed ",
    "the dog that worried ",
    "the cat that killed ",
    "the rat that ate ",
    "the malt that lay in ",
    "",
)


def recite(start_verse: int, end_verse: int) -> List[str]:
    """
    Recites verses from 'The House That Jack Built' starting from the specified
    start_verse to the end_verse.

    Parameters:
    - start_verse (int): The verse number to start from (1-indexed).
    - end_verse (int): The verse number to end at (1-indexed).

    Returns:
    - List[str]: A list of verses from start_verse to end_verse.
    """
    return [
        f"This is {''.join(TEXT[-n:])}the house that Jack built."
        for n in range(start_verse, end_verse + 1)
    ]
