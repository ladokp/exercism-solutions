"""Module to handle Tree data structures with labeled nodes and support various operations.

This module provides the Tree class, which allows for the representation of a tree
data structure where each node is represented as a Tree object. The Tree class offers
methods to convert the tree into a dictionary for serialization, produce a string
representation with JSON formatting, compare nodes, reorient the tree from the perspective
of a specified node, check for the existence of a node with a specific label, and find paths
between nodes.
"""

from json import dumps


class Tree:
    """A class representing a tree data structure with labeled nodes.

    Each node in the tree is represented as a Tree object, which contains a label and optional child nodes.
    The tree supports operations such as conversion to a dictionary for serialization, string representation
    with JSON formatting, comparison, and reorientation from the point of view of a specified node. It also
    provides methods to check for the existence of a node with a specific label and find paths between nodes.
    """

    def __init__(self, label: str, children: list["Tree"] = None) -> None:
        """Initialize a Tree node with a label and optional children.

        Args:
            label: The label of the node.
            children: A list of child Tree nodes.
        """
        self.label = label
        self.children = children if children is not None else []

    @property
    def __dict__(self) -> dict:
        """Convert the Tree node into a dictionary format for serialization.

        Returns:
            A dictionary representation of the Tree node.
        """
        return {self.label: [c.__dict__ for c in sorted(self.children)]}

    def __str__(self, indent: int = None) -> str:
        """Get the string representation of the Tree.

        Args:
            indent: Optional indentation level for pretty-printing.

        Returns:
            A JSON formatted string representation of the Tree.
        """
        return dumps(self.__dict__, indent=indent)

    def __lt__(self, other: "Tree") -> bool:
        """Compare two Tree nodes based on their labels.

        Args:
            other: Another Tree node to compare against.

        Returns:
            True if this node's label is less than the other node's label, False otherwise.
        """
        return self.label < other.label

    def __eq__(self, other: "Tree") -> bool:
        """Check if two Tree nodes are equal.

        Args:
            other: Another Tree node to compare against.

        Returns:
            True if both nodes have the same structure and labels, False otherwise.
        """
        return self.__dict__ == other.__dict__

    def from_pov(self, from_node: str) -> "Tree":
        """Reorient the tree from the point of view of a specified node.

        Args:
            from_node: The label of the node from which to view the tree.

        Returns:
            A Tree object reoriented from the perspective of the specified node.

        Raises:
            ValueError: If the tree cannot be reoriented to the specified node.
        """
        if self.label == from_node:
            return self
        if not (child := self.child_contains(from_node)):
            raise ValueError("Tree could not be reoriented")
        self.children.remove(child)
        child.children.append(self)
        return child.from_pov(from_node)

    def child_contains(self, node: str) -> "Tree":
        """Check if any of the child nodes contains a node with the specified label.

        Args:
            node: The label to search for.

        Returns:
            The child Tree node containing the node with the specified label, or None if not found.
        """
        return next(
            (child for child in self.children if child.contains_node(node)),
            None,
        )

    def contains_node(self, node: str) -> bool:
        """Determine if this Tree node or its descendants contain a node with the specified label.

        Args:
            node: The label to search for.

        Returns:
            True if a node with the specified label is found, False otherwise.
        """
        return self.label == node or any(
            child for child in self.children if child.contains_node(node)
        )

    def path_to(self, from_node: str, to_node: str) -> list[str]:
        """Find the path from one node to another in the tree.

        Args:
            from_node: The starting node label.
            to_node: The target node label.

        Returns:
            A list of labels representing the path from `from_node` to `to_node`.

        Raises:
            ValueError: If no path can be found.
        """
        pov = self.from_pov(from_node)

        def path(pov_: "Tree", to_node_: str):
            yield pov_.label
            while pov_.label != to_node_:
                pov_ = pov_.child_contains(to_node_)
                if not pov_:
                    raise ValueError("No path found")
                yield pov_.label

        return list(path(pov, to_node))
