"""
This module provides classes and functions to represent a user management system
with financial transactions, along with a RESTful API interface for interacting
with the data. It includes decorators for JSON handling and supports operations
such as adding users and recording loans and borrowings.
"""

from functools import wraps
import json


def json_io(func):
    """Decorator to handle JSON input and output for the decorated function."""

    @wraps(func)
    def decorator(self, url: str, payload: str = None, indent=2) -> str:
        if payload is not None and isinstance(payload, str):
            payload = json.loads(payload)
        return json.dumps(func(self, url, payload), indent=indent)

    return decorator


class EndpointNotImplementedException(Exception):
    """Exception raised when an endpoint is not implemented."""


class User:
    """Represents a user with loan and borrowing records."""

    def __init__(
        self,
        name: str,
        owed_by: dict = None,
        owes: dict = None,
        **_,
    ):
        """
        Initialize a User instance.

        :param name: The name of the user.
        :param owed_by: A dictionary of users who owe this user money.
        :param owes: A dictionary of users to whom this user owes money.
        :param kwargs: Additional keyword arguments.
        """
        self.name = name
        self.records = {}
        for borrower, amount in (owed_by or {}).items():
            self.loan(borrower, amount)
        for lender, amount in (owes or {}).items():
            self.borrow(lender, amount)

    def borrow(self, borrower: str, amount: float):
        """
        Record an amount borrowed from another user.

        :param borrower: The name of the borrower.
        :param amount: The amount borrowed.
        """
        self.records[borrower] = self.records.get(borrower, 0) - amount

    def loan(self, lender: str, amount: float):
        """
        Record an amount loaned to another user.

        :param lender: The name of the lender.
        :param amount: The amount loaned.
        """
        self.records[lender] = self.records.get(lender, 0) + amount

    @property
    def owes(self) -> dict:
        """Return a dictionary of amounts this user owes to others."""
        return {k: -v for k, v in self.records.items() if v < 0}

    @property
    def owed_by(self) -> dict:
        """Return a dictionary of amounts owed by others to this user."""
        return {k: v for k, v in self.records.items() if v > 0}

    @property
    def balance(self) -> float:
        """Calculate and return the balance of the user's loans and borrowings."""
        return sum(self.records.values())

    @property
    def __dict__(self) -> dict:
        """Return a dictionary representation of the user's financial status."""
        return {
            "name": self.name,
            "owes": self.owes,
            "owed_by": self.owed_by,
            "balance": self.balance,
        }


class RestAPI:
    """RESTful API class to handle user-related financial operations."""

    def __init__(self, database: dict = None):
        """
        Initialize an instance of RestAPI.

        :param database: A dictionary representing the initial database state, containing user data.
        """
        self.users = {
            user["name"]: User(**user)
            for user in (database or {}).get("users", [])
        }

    @json_io
    def get(self, url: str, payload: dict = None) -> dict:
        """
        Handle GET requests to the API.

        :param url: The endpoint of the API request.
        :param payload: The optional payload for the request.
        :returns: A JSON response string.
        """
        if url == "/users":
            return {
                "users": [
                    user.__dict__
                    for name, user in sorted(self.users.items())
                    if payload is None or name in payload["users"]
                ]
            }
        raise EndpointNotImplementedException(
            f"The endpoint {url} is not implemented."
        )

    @json_io
    def post(self, url: str, payload: dict) -> dict:
        """
        Handle POST requests to the API.

        :param url: The endpoint of the API request.
        :param payload: The payload for the request.
        :returns: A JSON response string.
        """
        if url == "/add":
            user = User(payload["user"])
            self.users[user.name] = user
            return user.__dict__
        if url == "/iou":
            lender = self.users[payload["lender"]]
            borrower = self.users[payload["borrower"]]
            amount = payload["amount"]
            lender.loan(borrower.name, amount)
            borrower.borrow(lender.name, amount)
            return {
                "users": sorted(
                    [lender.__dict__, borrower.__dict__],
                    key=lambda u: u["name"],
                )
            }
        raise EndpointNotImplementedException(
            f"The endpoint {url} is not implemented."
        )
