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
    """Calculate the modifier for a given ability score.

    Args:
        score (int): The ability score.

    Returns:
        int: The calculated modifier.
    """
    return (score - 10) // 2


class Character:
    """Represents a game character with various abilities and hit points."""

    def __init__(self):
        """Initialize a new character with random ability scores and hit points."""
        for ability in ABILITIES:
            setattr(self, ability, self.ability())
        self.hitpoints = 10 + modifier(self.constitution)

    @staticmethod
    def ability() -> int:
        """Roll four six-sided dice and return the sum of the three highest rolls.

        Returns:
            int: The ability score.
        """
        dices = sorted(secrets.randbelow(7) for _ in range(4))
        return sum(dices[1:])
