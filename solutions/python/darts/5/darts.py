"""
This module calculates a score based on the distance from the origin.

The score is determined by how close the given point (x, y) is to the origin (0, 0).
"""


def score(x_coordinate: float, y_coordinate: float) -> int:
    """
    Calculate the score based on the distance from the origin.

    Parameters:
    x_coordinate (float): The x-coordinate of the point.
    y_coordinate (float): The y-coordinate of the point.

    Returns:
    int: The calculated score.
    """
    distance_from_origin = (x_coordinate**2 + y_coordinate**2) ** 0.5
    scoring_zones = (
        distance_from_origin <= 1,
        distance_from_origin <= 5,
        distance_from_origin <= 10,
    )
    match scoring_zones:
        case (True, *_):
            return 10
        case (_, True, _):
            return 5
        case (*_, True):
            return 1
    return 0
