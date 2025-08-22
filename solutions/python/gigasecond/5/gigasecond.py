"""Module for adding one billion seconds to a given moment."""

from datetime import timedelta

giga_second = timedelta(seconds=1e9)


def add(moment):
    """Add one billion seconds to the given moment.

    Args:
        moment (datetime): The initial moment to which one billion seconds will be added.

    Returns:
        datetime: The moment after adding one billion seconds.
    """
    return moment + giga_second
