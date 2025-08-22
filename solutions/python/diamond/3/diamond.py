"""
This module contains a function to generate rows of letters up to a specified end letter.
"""


def rows(end_letter):
    """
    Generates a list of strings, each representing a row of letters up to
    the specified end letter.

    The rows are structured in such a way that they form a pattern with spaces
    where letters don't match.

    Parameters:
    end_letter (str): The ending letter for the rows, must be a single
    uppercase character.

    Returns:
    list: A list of strings containing the generated rows.
    """
    letters_tuple = tuple(
        chr(code_point) for code_point in range(ord("A"), ord(end_letter) + 1)
    )
    return [
        "".join(
            left_char if left_char == right_char else " "
            for right_char in tuple(letters_tuple[::-1] + letters_tuple[1:])
        )
        for left_char in tuple(letters_tuple[:-1] + letters_tuple[::-1])
    ]
