import secrets

ABILITIES = (
    "strength",
    "dexterity",
    "constitution",
    "intelligence",
    "wisdom",
    "charisma",
)


def modifier(score):
    return (score - 10) // 2


class Character:
    def __init__(self):
        for ability in ABILITIES:
            setattr(self, ability, self.ability())
        self.hitpoints = 10 + modifier(self.constitution)

    @staticmethod
    def ability():
        dices = sorted(secrets.randbelow(7) for _ in range(4))
        return sum(dices[1:])
