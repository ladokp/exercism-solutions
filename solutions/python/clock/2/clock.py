class Clock:
    MINUTES_OF_A_DAY = 24 * 60
    MINUTES_OF_AN_HOUR = 60

    def __init__(self, hour, minute):
        self.minutes = (
            hour * self.MINUTES_OF_AN_HOUR + minute
        ) % self.MINUTES_OF_A_DAY

    def _current_hour(self):
        return self.minutes // self.MINUTES_OF_AN_HOUR % 24

    def _current_minutes(self):
        return self.minutes % self.MINUTES_OF_AN_HOUR

    def __repr__(self):
        return f"{self.__class__.__name__}({self._current_hour()}, {self._current_minutes()})"

    def __str__(self):
        return f"{self._current_hour():0>2}:{self._current_minutes():0>2}"

    def __eq__(self, other):
        return self.minutes == other.minutes

    def __add__(self, minutes):
        minutes = self.minutes + minutes
        return Clock(
            minutes // self.MINUTES_OF_AN_HOUR,
            minutes % self.MINUTES_OF_AN_HOUR,
        )

    def __sub__(self, minutes):
        return self.__add__(-minutes)
