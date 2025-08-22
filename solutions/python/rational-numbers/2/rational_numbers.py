from __future__ import division


class Rational:
    """
    A class to represent a rational number, providing basic arithmetic operations.
    """

    def __init__(self, numerator, denominator):
        """
        Initialize a Rational instance with a numerator and a denominator.

        :param numerator: The numerator of the rational number.
        :param denominator: The denominator of the rational number, must not be zero.
        :raises ValueError: If the denominator is zero or either argument is not an integer.
        """
        if denominator == 0:
            raise ValueError("Denominator cannot be 0")
        if not (isinstance(numerator, int) and isinstance(denominator, int)):
            raise ValueError("Numerator and Denominator have to be integers")
        self.numerator = numerator
        self.denominator = denominator
        self.reduce()

    @staticmethod
    def gcd(number_one, number_two):
        """
        Calculate the greatest common divisor of two integers using Euclid's algorithm.

        :param number_one: An integer.
        :param number_two: Another integer.
        :return: The greatest common divisor of a and b.
        """
        while number_two != 0:
            number_one, number_two = number_two, number_one % number_two
        return abs(number_one)

    def reduce(self):
        """
        Reduce the rational number to its simplest form.

        :return: The Rational instance in its reduced form.
        """
        div = Rational.gcd(self.numerator, self.denominator)
        self.numerator //= div
        self.denominator //= div
        if self.denominator < 0:  # Ensure denominator is positive
            self.numerator *= -1
            self.denominator *= -1
        return self

    def __eq__(self, other):
        """
        Determine if two Rational instances are equal.

        :param other: Another Rational instance.
        :return: True if both instances represent the same rational number, otherwise False.
        """
        return (
            self.numerator == other.numerator
            and self.denominator == other.denominator
        )

    def __repr__(self):
        """
        Return a string representation of the Rational instance.

        :return: String representing the rational number as "numerator/denominator".
        """
        return f"{self.numerator}/{self.denominator}"

    def __add__(self, other):
        """
        Add two Rational instances.

        :param other: Another Rational instance.
        :return: A new Rational instance representing the sum.
        """
        return Rational(
            self.numerator * other.denominator
            + other.numerator * self.denominator,
            self.denominator * other.denominator,
        ).reduce()

    def __sub__(self, other):
        """
        Subtract another Rational instance from this one.

        :param other: Another Rational instance.
        :return: A new Rational instance representing the difference.
        """
        other.denominator *= -1
        return self + other

    def __mul__(self, other):
        """
        Multiply two Rational instances.

        :param other: Another Rational instance.
        :return: A new Rational instance representing the product.
        """
        return Rational(
            self.numerator * other.numerator,
            self.denominator * other.denominator,
        ).reduce()

    def __truediv__(self, other):
        """
        Divide this Rational instance by another.

        :param other: Another Rational instance.
        :return: A new Rational instance representing the quotient.
        :raises ZeroDivisionError: If attempting to divide by zero.
        """
        if other.numerator == 0:
            raise ZeroDivisionError("Cannot divide by zero")
        return Rational(
            self.numerator * other.denominator,
            self.denominator * other.numerator,
        ).reduce()

    def __abs__(self):
        """
        Return the absolute value of the Rational instance.

        :return: The Rational instance representing the absolute value.
        """
        return Rational(abs(self.numerator), abs(self.denominator))

    def __pow__(self, power):
        """
        Raise the Rational instance to a given power.

        :param power: The power to raise the rational number to.
        :return: A new Rational instance representing the power.
        """
        if power < 1:
            return Rational(self.denominator**-power, self.numerator**-power)
        return Rational(self.numerator**power, self.denominator**power)

    def __rpow__(self, base):
        """
        Implement the right power function for the Rational instance.

        :param base: The base number.
        :return: The result of raising the base to the power represented by the rational number.
        """
        return base ** (self.numerator / self.denominator)
