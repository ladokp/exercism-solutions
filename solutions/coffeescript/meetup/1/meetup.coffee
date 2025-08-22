###
  This module provides a way to determine a specific meetup date given:
    - A year
    - A month
    - A week descriptor (e.g., first, second, third, etc.)
    - A day of the week (e.g., Monday, Tuesday, etc.)
  
  The "week" refers to the first day of the month that could qualify for the specific week:
    - First: 1st day of the month
    - Second: 8th day of the month
    - Third: 15th day of the month
    - Fourth: 22nd day of the month
    - Teenth: 13th to 19th of the month
    - Last: The last occurrence of the day in the month

  The function uses JavaScript's `Date` object and assumes 0-based months (January = 0).
###

# Week starting points in the month (sentinel value for "Last")
Weeks =
  First: 1
  Second: 8
  Third: 15
  Fourth: 22
  Teenth: 13
  Last: -1  # Special case, sentinel value for the last occurrence

# JavaScript weekday numbers (Sunday = 0, Monday = 1, etc.)
Weekdays =
  Sunday: 0
  Monday: 1
  Tuesday: 2
  Wednesday: 3
  Thursday: 4
  Friday: 5
  Saturday: 6

###
  Determines the exact date of a meetup based on the provided parameters.

  @param {Object} options - an object containing:
    - year: the year of the meetup (4-digit integer)
    - month: the month of the meetup (1-based, e.g., January = 1)
    - week: the week descriptor (one of `Weeks` values)
    - dayofweek: the desired day of the week (one of `Weekdays` values)

  @return {Date} - a JavaScript Date object for the computed meetup date
###
meetup = ({year, month, week, dayofweek}) ->

  # Initialize the date to the first day of the given month
  # JavaScript months are 0-based, so we subtract 1 from `month`.
  # Set hour to 12 to avoid daylight saving time issues.
  date = new Date year, month - 1, 1, 12

  # Special case: if the week is "Last", calculate the start of the last week
  if week == Weeks.Last
    # The zeroth day of the next month gives the last day of this month
    # Subtracting 6 gives the start of the last possible week
    week = new Date(year, month, 0, 12).getDate() - 6

  # Loop through the days of the month starting from the beginning
  # until we find the correct day of the week and week number.
  loop
    d = date.getDate()

    # Check if the current date matches the desired weekday and week threshold
    if date.getDay() == dayofweek and d >= week
      # Return the matched date as a new Date object
      return new Date date.getFullYear(), date.getMonth(), d

    # Move to the next day and continue the search
    date.setDate d + 1

# Export the Weeks, Weekdays, and meetup function for external use
module.exports = {Weeks, Weekdays, meetup}
