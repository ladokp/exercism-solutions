from __future__ import division


class Rational(object):
    def __init__(self, numerator, denominator):
        if denominator == 0:
            raise ValueError("Denominator cannot be 0")
        if not (isinstance(numerator, int) and isinstance(denominator, int)):
            raise ValueError("Numerator and Denominator have to be integers")
        self.numerator = numerator
        self.denominator = denominator
        self.reduce()

    @staticmethod
    def gcd(a, b):
        if a == 0:
            return b
        if b == 0:
            return a
        return Rational.gcd(b, a % b)

    def reduce(self):
        div = Rational.gcd(self.numerator, self.denominator)
        self.numerator //= div
        self.denominator //= div
        return self

    def __eq__(self, other):
        return (
            self.numerator == other.numerator
            and self.denominator == other.denominator
        )

    def __repr__(self):
        return f"{self.numerator}/{self.denominator}"

    def __add__(self, other):
        return Rational(
            self.numerator * other.denominator
            + other.numerator * self.denominator,
            (self.denominator * other.denominator),
        )

    def __sub__(self, other):
        return Rational(
            self.numerator * other.denominator
            - other.numerator * self.denominator,
            (self.denominator * other.denominator),
        )

    def __mul__(self, other):
        return Rational(
            self.numerator * other.numerator,
            self.denominator * other.denominator,
        )

    def __truediv__(self, other):
        return Rational(
            self.numerator * other.denominator,
            self.denominator * other.numerator,
        )

    def __abs__(self):
        if self.numerator < 0:
            self.numerator *= -1
        if self.denominator < 0:
            self.denominator *= -1
        return self

    def __pow__(self, power):
        if power > 0:
            return Rational(self.numerator**power, self.denominator**power)
        return Rational(self.denominator**-power, self.numerator**-power)

    def __rpow__(self, base):
        return pow(base**self.numerator, 1 / self.denominator)
