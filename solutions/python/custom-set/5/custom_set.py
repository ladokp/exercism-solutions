from __future__ import annotations
from typing import Iterable, Any, Optional, Iterator


class CustomSet:
    """Create a CustomSet from any object(s)."""

    def __init__(self, elements: Optional[Any] = None):
        if elements is None:
            self.elements = []
        elif isinstance(elements, Iterable):
            self.elements = list(elements)
        else:
            self.elements = [elements]

    def isempty(self) -> bool:
        return len(self) == 0

    def __contains__(self, element: Any) -> bool:
        return element in self.elements

    def issubset(self, other: CustomSet) -> bool:
        return all(element in other for element in self)

    def isdisjoint(self, other: CustomSet) -> bool:
        return all(element not in other for element in self)

    def __eq__(self, other: CustomSet) -> bool:
        return self.issubset(other) and other.issubset(self)

    def add(self, element: Any) -> None:
        if element not in self:
            self.elements.append(element)

    def intersection(self, other: CustomSet) -> CustomSet:
        return CustomSet(element for element in other if element in self)

    def __sub__(self, other: CustomSet) -> CustomSet:
        return CustomSet(element for element in self if element not in other)

    def __add__(self, other: CustomSet) -> CustomSet:
        new_set = CustomSet(self)
        for element in other:
            new_set.add(element)
        return new_set

    def __iter__(self) -> Iterator[Any]:
        yield from self.elements

    def __len__(self) -> int:
        return len(self.elements)
