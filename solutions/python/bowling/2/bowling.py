class BowlingGame:
    def __init__(self) -> None:
        """Initialize the BowlingGame with an empty list of throws, 20 frames, and 0 bonus frames."""
        self.throws: list[int] = []
        self.frame: int = 20
        self.bonus_frame: int = 0

    def roll(self, pins: int) -> None:
        """Record a roll with the number of pins knocked down.

        Args:
            pins (int): The number of pins knocked down in this roll.

        Raises:
            ValueError: If the number of pins is not between 0 and 10.
            ValueError: If the conditions for a valid frame are not met.
        """
        if pins > 10 or pins < 0:
            raise ValueError("Pins should be inbetween 0 and 10")
        self.throws.append(pins)
        if self.bonus_frame:
            self.bonus_frame -= 1
            if all(
                (
                    self.bonus_frame == 0,
                    self.throws[-3] == 10,
                    self.throws[-2] < 10,
                    self.throws[-2] + self.throws[-1] > 10,
                )
            ):
                raise ValueError("There should only be 10 pins")
            return
        if pins == 10:
            self.frame -= 2
            if self.frame == 0:
                self.bonus_frame = 2
        else:
            self.frame -= 1
            if self.frame % 2 == 0 and self.throws[-2] + self.throws[-1] > 10:
                raise ValueError("There should only be 10 pins")
            if self.frame == 0 and self.throws[-2] + self.throws[-1] == 10:
                self.bonus_frame = 1
        if self.frame < 0:
            raise ValueError("Game has already ended")

    def score(self) -> int:
        """Calculate the total score for the game.

        Returns:
            int: The total score for the game.

        Raises:
            ValueError: If the game state is invalid for scoring.
        """
        if not 10 < len(self.throws) < 22 or self.bonus_frame != 0:
            raise ValueError("Invalid Game")
        point: int = 0
        for index, _ in enumerate(self.throws):
            if self.throws[index] == 10:
                self.frame += 2
                point += sum(self.throws[index : index + 3])
            else:
                self.frame += 1
                if (
                    self.frame % 2 == 0
                    and self.throws[index] + self.throws[index - 1] == 10
                ):
                    point += sum(self.throws[index : index + 2])
                else:
                    point += self.throws[index]
            if self.frame == 20:
                break
        return point
