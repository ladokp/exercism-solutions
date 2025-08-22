"""
This module provides a thread-safe implementation of a bank account with
basic operations such as opening, closing, depositing, and withdrawing funds.
"""

import threading


class BankAccount:
    """A class representing a bank account that supports basic operations such as
    opening, closing, depositing, and withdrawing funds. The account is thread-safe
    using a mutex to manage concurrent access."""

    def __init__(self):
        """Initialize a new bank account."""
        self.is_open = False
        self.balance = 0
        self.mutex = threading.Lock()

    def get_balance(self):
        """Get the current balance of the bank account.

        Raises:
            ValueError: If the account is not open.

        Returns:
            int: The current balance.
        """
        with self.mutex:
            if not self.is_open:
                raise ValueError("account not open")
            return self.balance

    def open(self):
        """Open the bank account.

        Raises:
            ValueError: If the account is already open.
        """
        if self.is_open:
            raise ValueError("account already open")
        self.is_open = True
        self.balance = 0

    def deposit(self, amount):
        """Deposit a specified amount into the bank account.

        Args:
            amount (int): The amount to deposit.

        Raises:
            ValueError: If the account is not open or amount is less than or equal to 0.
        """
        with self.mutex:
            if not self.is_open:
                raise ValueError("account not open")
            if amount <= 0:
                raise ValueError("amount must be greater than 0")
            self.balance += amount

    def withdraw(self, amount):
        """Withdraw a specified amount from the bank account.

        Args:
            amount (int): The amount to withdraw.

        Raises:
            ValueError: If the account is not open, amount is less than or equal to 0,
                        or amount is greater than the balance.
        """
        with self.mutex:
            if not self.is_open:
                raise ValueError("account not open")
            if amount <= 0:
                raise ValueError("amount must be greater than 0")
            if amount > self.balance:
                raise ValueError("amount must be less than balance")
            self.balance -= amount

    def close(self):
        """Close the bank account.

        Raises:
            ValueError: If the account is not open.
        """
        if not self.is_open:
            raise ValueError("account not open")
        self.is_open = False
