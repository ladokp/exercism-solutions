class Clock:
    """
    A class to represent a 24-hour clock.

    This class provides a way to model time using hours and minutes,
    allowing for basic arithmetic operations like addition and subtraction of minutes.
    The time is internally stored as the total number of minutes since midnight,
    normalized within a 24-hour day.

    Attributes:
        MINUTES_OF_AN_HOUR (int): Constant representing the number of minutes in an hour (60).
        HOURS_OF_A_DAY (int): Constant representing the number of hours in a day (24).
        MINUTES_OF_A_DAY (int): Constant representing the total number of minutes in a day (1440).

    Methods:
        _current_hour(): Returns the current hour based on total minutes.
        _current_minutes(): Returns the current minutes based on total minutes.
        __repr__(): Returns a string representation of the Clock object in the form 'Clock(hour, minute)'.
        __str__(): Returns a string representation of the time in the format 'HH:MM'.
        __eq__(other): Compares two Clock objects for equality based on total minutes.
        __add__(minutes): Adds a given number of minutes to the current time and returns a new Clock object.
        __sub__(minutes): Subtracts a given number of minutes from the current time and returns a new Clock object.
    """

    MINUTES_OF_AN_HOUR = 60
    HOURS_OF_A_DAY = 24
    MINUTES_OF_A_DAY = HOURS_OF_A_DAY * MINUTES_OF_AN_HOUR

    def __init__(self, hour, minute):
        """
        Initialize the Clock object with the given hour and minute.

        Args:
            hour (int): The hour component of the time.
            minute (int): The minute component of the time.
        """
        self.minutes = (
            hour * Clock.MINUTES_OF_AN_HOUR + minute
        ) % Clock.MINUTES_OF_A_DAY

    def _current_hour(self):
        """Calculate and return the current hour."""
        return self.minutes // Clock.MINUTES_OF_AN_HOUR % Clock.HOURS_OF_A_DAY

    def _current_minutes(self):
        """Calculate and return the current minutes."""
        return self.minutes % Clock.MINUTES_OF_AN_HOUR

    def __repr__(self):
        """Return a string representation of the Clock object."""
        return f"{self.__class__.__name__}({self._current_hour()}, {self._current_minutes()})"

    def __str__(self):
        """Return the time in the format 'HH:MM'."""
        return f"{self._current_hour():0>2}:{self._current_minutes():0>2}"

    def __eq__(self, other):
        """Compare this Clock object with another for equality."""
        return self.minutes == other.minutes

    def __add__(self, minutes):
        """
        Add a number of minutes to the current time and return a new Clock object.

        Args:
            minutes (int): The number of minutes to add.

        Returns:
            Clock: A new Clock object with the updated time.
        """
        minutes = self.minutes + minutes
        return Clock(
            minutes // Clock.MINUTES_OF_AN_HOUR,
            minutes % Clock.MINUTES_OF_AN_HOUR,
        )

    def __sub__(self, minutes):
        """
        Subtract a number of minutes from the current time and return a new Clock object.

        Args:
            minutes (int): The number of minutes to subtract.

        Returns:
            Clock: A new Clock object with the updated time.
        """
        return self.__add__(-minutes)
