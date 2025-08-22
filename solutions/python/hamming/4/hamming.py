"""
A module for calculating the Hamming Distance between two DNA strands.
"""

ERROR_MESSAGE = "Strands must be of equal length."


def distance(strand_a: str, strand_b: str) -> int:
    """
    Calculating the Hamming Distance between two DNA strands.
    """
    if not len(strand_a) - len(strand_b):
        return sum(
            a != b
            for a, b in zip(
                strand_a,
                strand_b,
            )
        )

    raise ValueError(ERROR_MESSAGE)
