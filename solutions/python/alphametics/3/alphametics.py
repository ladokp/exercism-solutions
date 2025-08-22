from itertools import permutations
from typing import Dict, Optional, List, Tuple


def parse_puzzle(puzzle: str) -> Tuple[List[str], str]:
    """Parse the puzzle string into a list of words and the resulting value.

    Args:
        puzzle (str): The puzzle in the form of "word1 + word2 + ... == result".

    Returns:
        Tuple[List[str], str]: A tuple containing a list of words and the resulting value.
    """
    inputs, value = puzzle.split(" == ")
    words = [i.strip() for i in inputs.split("+")]
    return words, value.strip()


def solve(puzzle: str) -> Optional[Dict[str, int]]:
    """Solve the given puzzle by finding a valid mapping of letters to digits.

    Args:
        puzzle (str): The puzzle in the form of "word1 + word2 + ... == result".

    Returns:
        Optional[Dict[str, int]]: A dictionary mapping letters to digits if a solution exists,
        or None if no solution can be found.
    """
    words, value = parse_puzzle(puzzle)
    nonzero = set([word[0] for word in words + [value] if len(word) > 1])
    letters = list(set("".join(words + [value])) - nonzero) + list(nonzero)
    for permutation in permutations("0123456789", len(letters)):
        conv_dict = dict(zip(letters, permutation))
        if "0" in permutation[-len(nonzero) :]:
            continue
        values = [int("".join(conv_dict[w] for w in word)) for word in words]
        summed = int("".join(conv_dict[v] for v in value))
        if sum(values) == summed:
            return {k: int(v) for k, v in conv_dict.items()}
