"""
Module for complex number operations.

This module defines a ComplexNumber class that supports basic arithmetic operations
and other functionalities typical of complex numbers.
"""

from math import cos, sin, exp
from typing import Self, Union


class ComplexNumber:
    """
    A class to represent a complex number.

    Attributes:
        real: The real part of the complex number.
        imaginary: The imaginary part of the complex number.
    """

    def __init__(self, real: Union[float, Self], imaginary: float) -> None:
        """
        Initialize a ComplexNumber with a real and imaginary part.

        Args:
            real: The real part of the complex number.
            imaginary: The imaginary part of the complex number.
        """
        self.real = real
        self.imaginary = imaginary

    def __eq__(self, other: Self) -> bool:
        """
        Check equality with another ComplexNumber.

        Args:
            other: The other object to compare against.

        Returns:
            True if equal, otherwise False.
        """
        return self.real == other.real and self.imaginary == other.imaginary

    def __add__(self, other: Self) -> Self:
        """
        Add another ComplexNumber to this one.

        Args:
            other: The other ComplexNumber to add.

        Returns:
            The sum as a new ComplexNumber.
        """
        other = _make_complex(other)
        return ComplexNumber(
            self.real + other.real, self.imaginary + other.imaginary
        )

    def __radd__(self, other: Self) -> Self:
        return self + other

    def __mul__(self, other: Self) -> Self:
        """
        Multiply this ComplexNumber by another.

        Args:
            other: The other ComplexNumber to multiply.

        Returns:
            The product as a new ComplexNumber.
        """
        other = _make_complex(other)
        return ComplexNumber(
            (self.real * other.real - self.imaginary * other.imaginary),
            (self.imaginary * other.real + self.real * other.imaginary),
        )

    def __rmul__(self, other: Self) -> Self:
        return self * other

    def __sub__(self, other: Self) -> Self:
        """
        Subtract another ComplexNumber from this one.

        Args:
            other: The other ComplexNumber to subtract.

        Returns:
            The difference as a new ComplexNumber.
        """
        other = _make_complex(other)
        return ComplexNumber(
            (self.real - other.real), (self.imaginary - other.imaginary)
        )

    def __rsub__(self, other: Self) -> Self:
        return (self * ComplexNumber(-1, 0)) + other

    def __truediv__(self, other: Self) -> Self:
        """
        Divide this ComplexNumber by another.

        Args:
            other: The other ComplexNumber to divide by.

        Returns:
            The quotient as a new ComplexNumber.
        """
        other = _make_complex(other)
        return ComplexNumber(
            (self.real * other.real + self.imaginary * other.imaginary)
            / (other.real**2 + other.imaginary**2),
            (other.real * self.imaginary - self.real * other.imaginary)
            / (other.real**2 + other.imaginary**2),
        )

    def __rtruediv__(self, other: Self) -> Self:
        other = _make_complex(other)
        return other / self

    def __abs__(self) -> float:
        """
        Return the absolute value (magnitude) of the ComplexNumber.

        Returns:
            The magnitude as a float.
        """
        return pow(self.real**2 + self.imaginary**2, 0.5)

    def conjugate(self) -> Self:
        """
        Return the conjugate of the ComplexNumber.

        Returns:
            The conjugate as a new ComplexNumber.
        """
        return ComplexNumber(self.real, -self.imaginary)

    def exp(self) -> Self:
        """
        Calculate the exponential function of the ComplexNumber.

        Returns:
            The result as a new ComplexNumber.
        """
        return ComplexNumber(
            round(cos(self.imaginary), 8) * exp(self.real),
            round(sin(self.imaginary), 8) * exp(self.real),
        )


def _make_complex(number: ComplexNumber) -> ComplexNumber:
    """
    Convert a number to a ComplexNumber if it is not already one.

    Args:
        number: The number to convert.

    Returns:
        A ComplexNumber instance.
    """
    if not isinstance(number, ComplexNumber):
        number = ComplexNumber(number, 0)
    return number
