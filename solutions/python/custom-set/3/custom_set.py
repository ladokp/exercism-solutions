from typing import Iterable, Any, Self


class EmptyCustomSet:
    """A class enabling the CustomSet to be initialized with None"""


class CustomSet:
    """A custom set implementation"""

    def __init__(
        self, elements: EmptyCustomSet | Iterable | Any = EmptyCustomSet
    ):
        if elements is EmptyCustomSet:
            elements = []
        try:
            self.elements = list(element for element in elements)
        except TypeError:
            self.elements = [
                elements
            ]  # in case no iterable is given add the element to a new list

    def isempty(self):
        return len(self) == 0

    def __contains__(self, element: Any):
        return element in self.elements

    def issubset(self, other: Self):
        return all(element in other for element in self)

    def isdisjoint(self, other: Self):
        return all(element not in other for element in self)

    def __eq__(self, other: Self):
        return self.issubset(other) and other.issubset(self)

    def add(self, element: Any):
        if element not in self:
            self.elements.append(element)

    def intersection(self, other: Self):
        return CustomSet(element for element in other if element in self)

    def __sub__(self, other: Self):
        return CustomSet(element for element in self if element not in other)

    def __add__(self, other: Self):
        new_set = CustomSet(self)
        for element in other:
            new_set.add(element)
        return new_set

    def __iter__(self):
        yield from self.elements

    def __len__(self):
        return len(self.elements)
