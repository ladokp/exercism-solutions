"""Custom Set implementation."""

from __future__ import annotations
from typing import Iterable, Any, Optional, Iterator


class CustomSet:
    """Create a CustomSet from any object(s)."""

    def __init__(self, elements: Optional[Any] = None) -> None:
        """Initialize the CustomSet with provided elements.

        Parameters:
            elements (Optional[Any]): Elements to initialize the set with.
        """
        if elements is None:
            self.elements = []
        elif isinstance(elements, Iterable):
            self.elements = list(elements)
        else:
            self.elements = [elements]

    def isempty(self) -> bool:
        """Check if the set is empty.

        Returns:
            bool: True if the set is empty, False otherwise.
        """
        return len(self) == 0

    def __contains__(self, element: Any) -> bool:
        """Check if an element is in the set.

        Parameters:
            element (Any): The element to check for presence in the set.

        Returns:
            bool: True if the element is in the set, False otherwise.
        """
        return element in self.elements

    def issubset(self, other: CustomSet) -> bool:
        """Check if this set is a subset of another set.

        Parameters:
            other (CustomSet): The set to compare against.

        Returns:
            bool: True if this set is a subset of the other, False otherwise.
        """
        return all(element in other for element in self)

    def isdisjoint(self, other: CustomSet) -> bool:
        """Check if this set has no elements in common with another set.

        Parameters:
            other (CustomSet): The set to compare against.

        Returns:
            bool: True if the sets are disjoint, False otherwise.
        """
        return all(element not in other for element in self)

    def __eq__(self, other: CustomSet) -> bool:
        """Check if two sets are equal.

        Parameters:
            other (CustomSet): The set to compare for equality.

        Returns:
            bool: True if the sets are equal, False otherwise.
        """
        return self.issubset(other) and other.issubset(self)

    def add(self, element: Any) -> None:
        """Add an element to the set if it's not already present.

        Parameters:
            element (Any): The element to add to the set.
        """
        if element not in self:
            self.elements.append(element)

    def intersection(self, other: CustomSet) -> CustomSet:
        """Return a new CustomSet with elements common to both sets.

        Parameters:
            other (CustomSet): The set to intersect with.

        Returns:
            CustomSet: A new CustomSet containing the intersection.
        """
        return CustomSet(element for element in other if element in self)

    def __sub__(self, other: CustomSet) -> CustomSet:
        """Return a new CustomSet with elements in this set but not in another.

        Parameters:
            other (CustomSet): The set to subtract from.

        Returns:
            CustomSet: A new CustomSet containing the difference.
        """
        return CustomSet(element for element in self if element not in other)

    def __add__(self, other: CustomSet) -> CustomSet:
        """Return a new CustomSet that is the union of this set and another.

        Parameters:
            other (CustomSet): The set to unite with.

        Returns:
            CustomSet: A new CustomSet containing the union.
        """
        new_set = CustomSet(self)
        for element in other:
            new_set.add(element)
        return new_set

    def __iter__(self) -> Iterator[Any]:
        """Return an iterator over the elements of the set.

        Returns:
            Iterator[Any]: An iterator for the elements in the set.
        """
        yield from self.elements

    def __len__(self) -> int:
        """Return the number of elements in the set.

        Returns:
            int: The number of elements in the set.
        """
        return len(self.elements)
