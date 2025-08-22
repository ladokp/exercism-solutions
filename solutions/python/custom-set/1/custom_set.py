class CustomSet:
    def __init__(self, elements=[]):
        self.elements = elements

    def isempty(self):
        return len(self.elements) == 0

    def __contains__(self, element):
        return element in self.elements

    def issubset(self, other):
        return all(element in self.elements for element in other)

    def isdisjoint(self, other):
        return not self.issubset(other)

    def __eq__(self, other):
        return self.issubset(other) and len(other) == len(self.elements)

    def add(self, element):
        if element not in self.elements:
            self.elements.append(element)

    def intersection(self, other):
        return CustomSet(
            [element for element in other if element in self.elements]
        )

    def __sub__(self, other):
        return CustomSet(
            [element for element in self.elements if element not in other]
        )

    def __add__(self, other):
        new_set = CustomSet(self.elements)
        for element in other:
            new_set.add(element)
        return new_set

    def __iter__(self):
        for element in self.elements:
            yield element

    def __len__(self):
        return len(self.elements)
