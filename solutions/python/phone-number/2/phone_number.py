"""
This module defines the PhoneNumber class, which provides functionality
to represent, validate, and format phone numbers. It includes methods
to sanitize input by removing invalid characters, validate the number's
length and digit specifications, and format the number into a standard
readable form.
"""


class PhoneNumber:
    """A class to represent and validate a phone number."""

    def __init__(self, number: str) -> None:
        """
        Initialize the PhoneNumber object.

        :param number: The phone number as a string.
        """
        self.number = number
        self._sanitize()
        self._validate_and_format()
        self.area_code = self.number[:3]

    def _sanitize(self) -> None:
        """
        Remove non-digit characters from the phone number and validate content.

        Raises:
            ValueError: If the phone number contains letters or punctuation.
        """
        if any(ch.isalpha() for ch in self.number):
            raise ValueError("letters not permitted")
        if any(ch in "!@#$%^,/?_" for ch in self.number):
            raise ValueError("punctuations not permitted")
        self.number = "".join(ch for ch in self.number if ch.isdigit())

    def _validate_and_format(self) -> None:
        """
        Validate and format the phone number.

        Raises:
            ValueError: If the phone number does not meet length or digit starting conditions.
        """
        if len(self.number) < 10:
            raise ValueError("must not be fewer than 10 digits")
        if len(self.number) > 11:
            raise ValueError("must not be greater than 11 digits")
        if len(self.number) == 11:
            if self.number[0] != "1":
                raise ValueError("11 digits must start with 1")
            self.number = self.number[1:]
        for dig, num in (("0", "zero"), ("1", "one")):
            for ind, code in ((0, "area"), (3, "exchange")):
                if self.number[ind] == dig:
                    raise ValueError(f"{code} code cannot start with {num}")

    def pretty(self) -> str:
        """
        Format the phone number into a standard representation.

        :return: A formatted phone number string.
        """
        return f"({self.area_code})-{self.number[3:6]}-{self.number[6:]}"
