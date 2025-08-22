from json import dumps


class Tree:
    def __init__(self, label, children=None):
        self.label = label
        self.children = children if children is not None else []

    def __dict__(self):
        return {self.label: [c.__dict__() for c in sorted(self.children)]}

    def __str__(self, indent=None):
        return dumps(self.__dict__(), indent=indent)

    def __lt__(self, other):
        return self.label < other.label

    def __eq__(self, other):
        return self.__dict__() == other.__dict__()

    def from_pov(self, from_node):
        if self.label == from_node:
            return self
        else:
            if not (child := self.child_contains(from_node)):
                raise ValueError("Tree could not be reoriented")
            self.children.remove(child)
            child.children.append(self)
            return child.from_pov(from_node)

    def child_contains(self, node):
        return next(
            (child for child in self.children if child.contains_node(node)),
            None,
        )

    def contains_node(self, node):
        return self.label == node or any(
            child for child in self.children if child.contains_node(node)
        )

    def path_to(self, from_node, to_node):
        pov = self.from_pov(from_node)

        def path(pov_, to_node_):
            yield pov_.label
            while pov_.label != to_node_:
                pov_ = pov_.child_contains(to_node_)
                if not pov_:
                    raise ValueError("No path found")
                yield pov_.label

        return list(path(pov, to_node))
