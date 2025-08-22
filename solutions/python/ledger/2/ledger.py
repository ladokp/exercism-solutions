"""
Module for handling financial ledger entries and formatting them
based on locale and currency preferences.
"""

from collections import namedtuple
from datetime import datetime

LedgerEntry = namedtuple(
    "LedgerEntry",
    [
        "date",
        "description",
        "change",
    ],
)
"""
A named tuple to represent an entry in a ledger with the fields:
date, description and change.
"""


def create_entry(date, description, change):
    """Create a ledger entry with the specified date, description, and change amount."""
    return LedgerEntry(datetime.strptime(date, "%Y-%m-%d"), description, change)


LABELS = {
    "en_US": {
        "date": "Date",
        "description": "| Description",
        "change": "| Change",
        "positive_change": "{}{} ",
        "negative_change": "({}{})",
    },
    "nl_NL": {
        "date": "Datum",
        "description": "| Omschrijving",
        "change": "| Verandering",
        "positive_change": "{} {} ",
        "negative_change": "{} {} ",
    },
}


def format_entries(currency, locale, entries):
    """
    Format ledger entries into a table representation.

    Args:
        currency (str): The currency symbol key ("USD", "EUR").
        locale (str): The locale code ("en_US", "nl_NL").
        entries (list): A list of LedgerEntry objects.

    Returns:
        str: A formatted string representing the ledger entries.
    """
    # Retrieve label names according to the specified locale
    labels = LABELS.get(locale)

    # Initialize the table header with appropriate labels
    table = (
        f"{labels.get('date'):<11}"
        f"{labels.get('description'):<28}"
        f"{labels.get('change'):<15}"
    )

    # Process each entry in order of date and change amount
    for entry in sorted(entries, key=lambda e: (e.date, e.change)):
        # Format description to fit within specified character limit
        description = (
            entry.description
            if len(entry.description) <= 22
            else (entry.description[:22] + "...")
        )

        # Format the date based on locale
        if locale == "nl_NL":
            table += f"\n{entry.date:%d-%m-%Y} | {description:<25} | "
        else:
            table += f"\n{entry.date:%m/%d/%Y} | {description:<25} | "

        # Determine currency symbol
        currency_ = {
            "USD": "$",
            "EUR": "€",
        }.get(currency)

        # Format the change value
        change = f"{entry.change / 100:,.2f}"
        if locale == "nl_NL":
            change = (
                change.replace(",", ";").replace(".", ",").replace(";", ".")
            )

        # Apply specific formatting for negative changes if in 'en_US' locale
        if entry.change < 0 and locale == "en_US":
            change = labels.get("negative_change").format(currency_, change[1:])
        else:
            change = labels.get("positive_change").format(currency_, change)

        # Append the formatted change to the table row
        table += f"{change:>13}"

    # Return the complete formatted table
    return table
