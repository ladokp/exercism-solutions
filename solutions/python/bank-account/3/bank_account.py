"""
This module provides a thread-safe implementation of a bank account that
supports basic operations such as opening, closing, depositing, and withdrawing
funds while ensuring proper synchronization for concurrent access.
"""

import threading


class BankAccount:
    """A class representing a bank account that supports basic operations, including
    opening, closing, depositing, and withdrawing funds. This class ensures thread-safety
    through the use of a mutex, which manages concurrent access to the account balance.
    """

    def __init__(self):
        """Initialize a new bank account with an initial closed status and a zero balance."""
        self.is_open = False
        self.balance = 0
        self.mutex = threading.Lock()

    def get_balance(self):
        """Retrieve the current balance of the bank account.

        Raises:
            ValueError: If the account is not open.

        Returns:
            int: The current balance of the bank account.
        """
        with self.mutex:
            if not self.is_open:
                raise ValueError("account not open")
            return self.balance

    def open(self):
        """Open the bank account, allowing deposits and withdrawals.

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
            ValueError: If the account is not open or if the amount is less than or equal to 0.
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
            ValueError: If the account is not open, if the amount is less than or equal to 0,
                        or if the amount exceeds the available balance.
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
        """Close the bank account, preventing further transactions.

        Raises:
            ValueError: If the account is not open.
        """
        if not self.is_open:
            raise ValueError("account not open")
        self.is_open = False
