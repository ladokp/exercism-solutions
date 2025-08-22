from __future__ import annotations
from typing import Iterable, Any, Optional, Iterator


class CustomSet:
    """Create a CustomSet from any object(s)."""

    def __init__(self, elements: Optional[Any] = None) -> None:
        if elements is None:
            self.elements = []
        elif isinstance(elements, Iterable):
            self.elements = list(elements)
        else:
            self.elements = [elements]

    def isempty(self) -> bool:
        """Check if the set is empty."""
        return len(self) == 0

    def __contains__(self, element: Any) -> bool:
        """Check if an element is in the set."""
        return element in self.elements

    def issubset(self, other: CustomSet) -> bool:
        """Check if this set is a subset of another set."""
        return all(element in other for element in self)

    def isdisjoint(self, other: CustomSet) -> bool:
        """Check if this set has no elements in common with another set."""
        return all(element not in other for element in self)

    def __eq__(self, other: CustomSet) -> bool:
        """Check if two sets are equal."""
        return self.issubset(other) and other.issubset(self)

    def add(self, element: Any) -> None:
        """Add an element to the set if it's not already present."""
        if element not in self:
            self.elements.append(element)

    def intersection(self, other: CustomSet) -> CustomSet:
        """Return a new CustomSet with elements common to both sets."""
        return CustomSet(element for element in other if element in self)

    def __sub__(self, other: CustomSet) -> CustomSet:
        """Return a new CustomSet with elements in this set but not in another."""
        return CustomSet(element for element in self if element not in other)

    def __add__(self, other: CustomSet) -> CustomSet:
        """Return a new CustomSet that is the union of this set and another."""
        new_set = CustomSet(self)
        for element in other:
            new_set.add(element)
        return new_set

    def __iter__(self) -> Iterator[Any]:
        """Return an iterator over the elements of the set."""
        yield from self.elements

    def __len__(self) -> int:
        """Return the number of elements in the set."""
        return len(self.elements)
