"""
This module provides a function to convert a binary string into a list of handshake actions.
"""

ACTIONS = ("wink", "double blink", "close your eyes", "jump")


def commands(binary_str):
    """Convert a binary string to a list of handshake actions.

    Args:
        binary_str (str): A string representing a binary number.

    Returns:
        list: A list of actions corresponding to the binary input.

    Example:
        >>> commands('00001')
        ['wink']
        >>> commands('00110')
        ['double blink', 'close your eyes']
    """
    number = int(binary_str, 2)
    handshake = [
        action for action in ACTIONS if number & 2 ** ACTIONS.index(action)
    ]
    return handshake[::-1] if number & 16 else handshake
