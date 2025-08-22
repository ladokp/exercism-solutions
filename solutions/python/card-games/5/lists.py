"""Functions for managing poker hands and performing various card-related tasks.

Python list documentation: https://docs.python.org/3/tutorial/datastructures.html
"""


def get_rounds(number):
    """Generate a list of the current round number and the next two rounds.

    :param number: int - the current round number.
    :return: list - a list containing the current round number and the next two round numbers.
    """

    return list(range(number, number + 3))


def concatenate_rounds(rounds_1, rounds_2):
    """Combine two lists of round numbers into one.

    :param rounds_1: list - the first list of rounds played.
    :param rounds_2: list - the second list of rounds played.
    :return: list - a single list containing all rounds played from both input lists.
    """

    return [*rounds_1, *rounds_2]


def list_contains_round(rounds, number):
    """Determine if a specific round number is in the list of rounds played.

    :param rounds: list - the list of rounds played.
    :param number: int - the round number to check.
    :return: bool - True if the round number is in the list, False otherwise.
    """

    return number in rounds


def card_average(hand):
    """Calculate the average value of the cards in a hand.

    :param hand: list - the list of card values in hand.
    :return: float - the average value of the cards.
    """

    return sum(hand) / len(hand)


def approx_average_is_average(hand):
    """Check if the average of the hand is approximately equal to either the average of the first and last card or the middle card.

    :param hand: list - the list of card values in hand.
    :return: bool - True if either the average of the first and last card or the middle card equals the actual average, False otherwise.
    """

    return card_average(hand) in (
        card_average((hand[0], hand[-1])),
        float(hand[len(hand) // 2]),
    )


def average_even_is_average_odd(hand):
    """Check if the average of the cards at even indices equals the average of the cards at odd indices.

    :param hand: list - the list of card values in hand.
    :return: bool - True if the averages of the even-indexed and odd-indexed cards are equal, False otherwise.
    """

    return card_average(hand[::2]) == card_average(hand[1::2])


def maybe_double_last(hand):
    """Double the value of the last card if it is a Jack (value of 11).

    :param hand: list - the list of card values in hand.
    :return: list - the hand with the last card's value doubled if it is a Jack.
    """

    if (hand[-1]) == 11:
        hand[-1] *= 2
    return hand
