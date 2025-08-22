class SpaceAge:
    """A class to represent the age of a person in different planetary years."""

    def __init__(self, seconds: float) -> None:
        """Initialize the SpaceAge object with age in seconds."""
        self.age_in_seconds: float = seconds

    def on_mercury(self) -> float:
        """Calculate age on Mercury in Earth years."""
        return round(self.on_earth() / 0.2408467, 2)

    def on_venus(self) -> float:
        """Calculate age on Venus in Earth years."""
        return round(self.age_in_seconds / 31557600 / 0.61519726, 2)

    def on_earth(self, precision: int = 2) -> float:
        """Calculate age on Earth in Earth years."""
        return round(self.age_in_seconds / 31557600, precision)

    def on_mars(self) -> float:
        """Calculate age on Mars in Earth years."""
        return round(self.on_earth() / 1.8808158, 2)

    def on_jupiter(self) -> float:
        """Calculate age on Jupiter in Earth years."""
        return round(self.on_earth() / 11.862615, 2)

    def on_saturn(self) -> float:
        """Calculate age on Saturn in Earth years."""
        return round(self.on_earth() / 29.447498, 2)

    def on_uranus(self) -> float:
        """Calculate age on Uranus in Earth years."""
        return round(self.on_earth() / 84.016846, 2)

    def on_neptune(self) -> float:
        """Calculate age on Neptune in Earth years."""
        return round(self.on_earth() / 164.79132, 2)
