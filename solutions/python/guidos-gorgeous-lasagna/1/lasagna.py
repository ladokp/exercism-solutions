"""Functions used in preparing Guido's gorgeous lasagna.

Learn about Guido, the creator of the Python language: https://en.wikipedia.org/wiki/Guido_van_Rossum
"""

EXPECTED_BAKE_TIME = 40
PREPARATION_TIME = 2


def bake_time_remaining(elapsed_bake_time):
    """Calculate the bake time remaining.

    :param elapsed_bake_time: int - baking time already elapsed.
    :return: int - remaining bake time (in minutes) derived from 'EXPECTED_BAKE_TIME'.

    Function that takes the actual minutes the lasagna has been in the oven as
    an argument and returns how many minutes the lasagna still needs to bake
    based on the `EXPECTED_BAKE_TIME`.
    """

    return EXPECTED_BAKE_TIME - elapsed_bake_time


def preparation_time_in_minutes(number_of_layers):
    """Calculate the preparation time for a number of layers.

    :param number_of_layers: int - number of lasagne layers.
    :return: int - preparation time of a number of layers derived from 'PREPARATION_TIME'.

    Function that takes the actual layers of the lasagna as an argument and returns 
    how many minutes the preparation will take
    based on the `PREPARATION_TIME` of a single layer.
    """
    
    return PREPARATION_TIME * number_of_layers


def elapsed_time_in_minutes(number_of_layers, elapsed_bake_time):
    """Calculate the total time taken to prepare and bake the lasagna.

    :param number_of_layers: int - number of lasagne layers.
    :param elapsed_bake_time: int - baking time already elapsed.
    :return: int - taken time to prepare and bake the lasgna.

    Function that takes the actual layers of the lasagna and the actual minutes the lasagna 
    has been in the oven as arguments and returns how many minutes the preparation and
    baking already took.
    """
    
    return preparation_time_in_minutes(number_of_layers) + elapsed_bake_time
    