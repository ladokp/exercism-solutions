class SpaceAge:
    ORBITAL_PERIODS = [
        (k, v * 31557600)
        for k, v in (
            ("earth", 1.0),
            ("mercury", 0.2408467),
            ("venus", 0.61519726),
            ("mars", 1.8808158),
            ("jupiter", 11.862615),
            ("saturn", 29.447498),
            ("uranus", 84.016846),
            ("neptune", 164.79132),
        )
    ]

    def __init__(self, seconds):
        self.seconds = seconds
        for planet, period in self.ORBITAL_PERIODS:
            setattr(self, "on_" + planet, lambda p=period: round(seconds / p, 2))
