"""Module to calculate the date of a specific weekday in a given month."""

import calendar
from datetime import date

weekdays = {
    "Monday": 0,
    "Tuesday": 1,
    "Wednesday": 2,
    "Thursday": 3,
    "Friday": 4,
    "Saturday": 5,
    "Sunday": 6,
}
nth = {"first": 1, "second": 2, "third": 3, "fourth": 4, "fifth": 5}


class MeetupDayException(ValueError):
    """Exception raised when the requested meetup day does not exist."""

    def __init__(self, message):
        self.message = message


def meetup(year, month, week, day_of_week):
    """
    Calculate the date of a specific weekday in a given year and month.

    Args:
        year (int): Year for the meetup.
        month (int): Month for the meetup.
        week (str): The week identifier ('first', 'second', 'teenth', 'last').
        day_of_week (str): Day of the week ('Monday', 'Tuesday', etc.).

    Returns:
        date: The calculated date of the meetup.

    Raises:
        MeetupDayException: If the requested day does not exist.
    """
    weekday_on_1st, num_days_in_month = calendar.monthrange(year, month)
    target_weekday = weekdays[day_of_week]
    first_target_weekday_date = 1 + ((target_weekday - weekday_on_1st) % 7)
    target_weekday_date = None
    if week in nth:
        target_weekday_date = first_target_weekday_date + ((nth[week] - 1) * 7)
        if target_weekday_date > num_days_in_month:
            raise MeetupDayException("That day does not exist.")
    if week == "teenth":
        if first_target_weekday_date + 7 >= 13:
            target_weekday_date = first_target_weekday_date + 7
        else:
            target_weekday_date = first_target_weekday_date + 14
    if week == "last":
        if first_target_weekday_date + 7 * 4 <= num_days_in_month:
            target_weekday_date = first_target_weekday_date + 7 * 4
        else:
            target_weekday_date = first_target_weekday_date + 7 * 3
    return date(year, month, target_weekday_date)
