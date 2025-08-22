import re


class Luhn:
    """
    A class to validate credit card numbers using the Luhn algorithm.

    Attributes:
        card_number (str): The credit card number as a string, with spaces removed.
        has_invalid_characters (bool): A flag indicating if the card number contains
                                       invalid characters or is too short.
        total (int): The cumulative total calculated from the digits of the card number.

    Methods:
        valid() -> bool: Validates the card number using the Luhn algorithm,
                         returning True if valid, False otherwise.
    """

    def __init__(self, card_number: str):
        """
        Initializes the Luhn class with a card number.

        Args:
            card_number (str): The card number as a string, which may contain spaces.
        """
        self.card_number = re.sub(r"\s", "", card_number)
        self.has_invalid_characters = (
            not self.card_number.isdigit() or len(self.card_number) <= 1
        )
        self.total = 0

    def valid(self):
        """
        Validates the card number using the Luhn algorithm.

        Returns:
            bool: True if the card number is valid, False otherwise.
        """
        if self.has_invalid_characters:
            return False
        if self.total == 0:
            for index, digit in enumerate(self.card_number[::-1]):
                digit = int(digit)
                if index & 1:
                    digit *= 2
                    if digit > 9:
                        digit -= 9
                self.total += digit
        return not self.total % 10
