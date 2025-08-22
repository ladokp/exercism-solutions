"""
A function to report if a given year is a leap year.
"""


def generate_leap_years(limit_year):
    return (
        year
        for year in range(1, limit_year + 1)
        if year % 4 == 0 and year % 100 != 0 or year % 400 == 0
    )


def leap_year(year):
    return year in generate_leap_years(year)
