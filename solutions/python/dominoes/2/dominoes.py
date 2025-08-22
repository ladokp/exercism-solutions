"""
This module contains a function to determine if a list of dominoes can form a chain.
"""


def can_chain(dominoes, chain=None):
    """
    Determine if the given list of dominoes can form a chain where each domino's
    end matches the beginning of the next one.

    Args:
        dominoes (list): A list of tuples representing the dominoes.
        chain (list, optional): The current chain being formed. Defaults to None.

    Returns:
        list: A chain of dominoes if possible, otherwise an empty list.
    """
    if chain is None:
        chain = []
    if not dominoes:
        return chain
    last_number = chain[-1][-1] if chain else dominoes[0][-1]
    for index, current_domino in enumerate(dominoes):
        if last_number in current_domino:
            attempted_chain = can_chain(
                dominoes[:index] + dominoes[index + 1 :],
                chain
                + [
                    (
                        current_domino
                        if current_domino[0] == last_number
                        else current_domino[::-1]
                    )
                ],
            )
            if (
                attempted_chain
                and attempted_chain[0][0] == attempted_chain[-1][-1]
            ):
                return attempted_chain
