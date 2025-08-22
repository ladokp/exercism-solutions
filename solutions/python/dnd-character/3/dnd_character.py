"""Module for character creation and ability score calculation."""

import secrets

ABILITIES = (
    "strength",
    "dexterity",
    "constitution",
    "intelligence",
    "wisdom",
    "charisma",
)


def modifier(score: int) -> int:
    """Calculate the ability score modifier based on the given score.

    Args:
        score (int): The ability score.

    Returns:
        int: The calculated modifier corresponding to the ability score.
    """
    return (score - 10) // 2


class Character:
    """Class representing a game character with defined abilities and hit points."""

    def __init__(self):
        """Initialize a new character with random ability scores and calculate hit points."""
        for ability in ABILITIES:
            setattr(self, ability, self.ability())
        self.hitpoints = 10 + modifier(self.constitution)

    @staticmethod
    def ability() -> int:
        """Roll four six-sided dice and return the total of the three highest rolls.

        Returns:
            int: The computed ability score derived from the dice rolls.
        """
        dices = sorted(secrets.randbelow(7) for _ in range(4))
        return sum(dices[1:])
