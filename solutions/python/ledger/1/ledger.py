# -*- coding: utf-8 -*-
from datetime import datetime


class LedgerEntry:
    def __init__(self, date, description, change):
        self.date = date
        self.description = description
        self.change = change


def create_entry(date, description, change):
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
    labels = LABELS.get(locale)
    table = (
        f"{labels.get('date'):<11}"
        f"{labels.get('description'):<28}"
        f"{labels.get('change'):<15}"
    )

    for entry in sorted(entries, key=lambda e: (e.date, e.change)):
        description = (
            entry.description
            if len(entry.description) <= 22
            else (entry.description[:22] + "...")
        )
        if locale == "nl_NL":
            table += f"\n{entry.date:%d-%m-%Y} | {description:<25} | "
        else:
            table += f"\n{entry.date:%m/%d/%Y} | {description:<25} | "

        currency_ = {
            "USD": "$",
            "EUR": "€",
        }.get(currency)
        change = f"{entry.change / 100:,.2f}"
        if locale == "nl_NL":
            change = (
                change.replace(",", ";").replace(".", ",").replace(";", ".")
            )
        if entry.change < 0 and locale == "en_US":
            change = labels.get("negative_change").format(currency_, change[1:])
        else:
            change = labels.get("positive_change").format(currency_, change)
        table += f"{change:>13}"
    return table
