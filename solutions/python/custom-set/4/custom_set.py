from typing import Iterable, Any, Self, Optional, Iterator


class EmptyCustomSet:
    """A class enabling the CustomSet to be initialized with None"""


class CustomSet:
    """A custom set implementation"""

    def __init__(self, elements: Optional[Any] = EmptyCustomSet):
        if elements is EmptyCustomSet:
            self.elements = []
        else:
            try:
                _ = (
                    element for element in elements
                )  # just check if given object is iterable
            except TypeError:
                self.elements = [
                    elements
                ]  # in case no iterable is given add the element to a new list
            else:
                self.elements = list(elements)

    def isempty(self) -> bool:
        return len(self) == 0

    def __contains__(self, element: Any) -> bool:
        return element in self.elements

    def issubset(self, other: Self) -> bool:
        return all(element in other for element in self)

    def isdisjoint(self, other: Self) -> bool:
        return all(element not in other for element in self)

    def __eq__(self, other: Self) -> bool:
        return self.issubset(other) and other.issubset(self)

    def add(self, element: Any) -> None:
        if element not in self:
            self.elements.append(element)

    def intersection(self, other: Self) -> Self:
        return CustomSet(element for element in other if element in self)

    def __sub__(self, other: Self) -> Self:
        return CustomSet(element for element in self if element not in other)

    def __add__(self, other: Self) -> Self:
        new_set = CustomSet(self)
        for element in other:
            new_set.add(element)
        return new_set

    def __iter__(self) -> Iterator[Any]:
        yield from self.elements

    def __len__(self) -> int:
        return len(self.elements)
