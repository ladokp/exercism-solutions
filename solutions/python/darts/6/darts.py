"""
This module calculates a score based on the distance from the origin.

The score is determined by how close the given point (x, y) is to the origin (0, 0).
"""


def score(x: float, y: float) -> int:
    """
    Calculate the score based on the distance from the origin.

    Parameters:
    x (float): The x-coordinate of the point.
    y (float): The y-coordinate of the point.

    Returns:
    score (int): The calculated score.
    """

    score = 0
    match (
        (distance_from_origin := (x**2 + y**2) ** 0.5) <= 1,
        distance_from_origin <= 5,
        distance_from_origin <= 10,
    ):
        case (True, *_):
            score = 10
        case (_, True, _):
            score = 5
        case (*_, True):
            score = 1  
    return score
