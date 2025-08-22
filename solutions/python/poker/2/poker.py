"""
Poker Hand Evaluation Module

This module provides functions to evaluate and rank poker hands.
It includes functionality to serialize hands, determine the best hand,
and compute ranks of cards within a hand.

Functions:
- best_hands: Determine the best poker hands from a list of hands.
- serialize: Convert hands from string format to a list of card tuples.
- deserialize: Convert serialized hands back to string format.
- hand_rank: Assigns a rank to a hand of poker cards.
- card_ranks: Computes the ranks of the cards in a hand.
"""


def best_hands(hands: list[str]) -> list[str]:
    """
    Determine the best poker hands from a list of hands.

    Args:
        hands (list of str): A list where each element is a string representing a hand of cards.

    Returns:
        list of str: A list of strings, each representing one of the best hands.
    """
    hands = serialize(hands)
    return [
        deserialize(hand)
        for hand in hands
        if card_ranks(hand) == card_ranks(max(hands, key=hand_rank))
    ]


def serialize(hands: list[str]) -> list[list[tuple[str, str]]]:
    """
    Convert a list of hand strings into a list of lists of tuples,
    where each tuple represents a card.

    Args:
        hands (list of str): A list of card hands in string format.

    Returns:
        list of list of tuple: A serialized form of the hands.
    """

    return [[(card[-2], card[-1]) for card in hand.split()] for hand in hands]


def deserialize(hand: list[tuple[str, str]]) -> str:
    """
    Convert a serialized hand back into a string format.

    Args:
        hand (list of tuple): A serialized hand of cards.

    Returns:
        str: A string representing the hand of cards.
    """
    return " ".join(["".join(["10" if r == "0" else r, s]) for r, s in hand])


def hand_rank(hand: list[tuple]) -> tuple[int, int]:
    """
    Assign a rank to a hand of poker cards.

    Args:
        hand (list of tuple): A hand of cards represented as a list of tuples.

    Returns:
        tuple: A tuple indicating the rank and structure of the hand.
    """
    ranks = card_ranks(hand)
    groups = [(ranks.count(i), i) for i in set(ranks)]
    groups.sort(reverse=True)
    counts, number = zip(*groups)
    straight = (len(counts) == 5) and (max(number) - min(number) == 4)
    flush = len({s for r, s in hand}) == 1

    match (straight, flush, counts):
        case (True, True, _):
            rank = 8
        case (_, _, (4, 1)):
            rank = 7
        case (_, _, (3, 2)):
            rank = 6
        case (_, True, _):
            rank = 5
        case (True, _, _):
            rank = 4
        case (_, _, (3, 1, 1)):
            rank = 3
        case (_, _, (2, 2, 1)):
            rank = 2
        case (_, _, (2, 1, 1, 1)):
            rank = 1
        case _:
            rank = 0

    return rank, number


def card_ranks(hand: list[tuple]) -> list[int]:
    """
    Compute the ranks of the cards in a hand.

    Args:
        hand (list of tuple): A hand of cards represented as a list of tuples.

    Returns:
        list of int: A list of integers representing the ranks of the cards.
    """
    ranks = ["234567890JQKA".index(r) + 2 for r, s in hand]
    ranks.sort(reverse=True)
    return [5, 4, 3, 2, 1] if (ranks == [14, 5, 4, 3, 2]) else ranks
