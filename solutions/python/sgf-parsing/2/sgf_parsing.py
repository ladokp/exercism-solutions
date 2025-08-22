"""
Module for parsing SGF (Smart Game Format) trees.

This module provides functionality to parse SGF-formatted strings into
a tree structure, represented by the SgfTree class.
"""


class SgfTree:
    """
    Represents a node in an SGF tree.

    Attributes:
        properties (dict): A dictionary of properties associated with this node.
        children (list): A list of child nodes.
    """

    def __init__(self, properties=None, children=None):
        """
        Initialize an SgfTree node.

        Args:
            properties (dict, optional): Initial properties for the node.
            children (list, optional): Initial child nodes for the node.
        """
        self.properties, self.children = properties or {}, children or []

    def __eq__(self, other):
        """
        Check if two SgfTree nodes are equal.

        Args:
            other (SgfTree): The node to compare against.

        Returns:
            bool: True if the nodes are equal, False otherwise.
        """
        return (
            self.properties == other.properties
            and self.children == other.children
        )


def parse(input_):
    """
    Parse an SGF string into an SgfTree.

    Args:
        input_ (str): The SGF-formatted string to parse.

    Returns:
        SgfTree: The root node of the parsed SGF tree.

    Raises:
        ValueError: If the input is not a valid SGF string.
    """
    position = 1

    def get_property():
        """
        Extract a property from the current position in the input string.

        Returns:
            dict: A dictionary containing the property key and values.

        Raises:
            ValueError: If the property delimiter is missing or the key is not uppercase.
        """
        nonlocal position
        index = input_[position:].find("[")
        if index == -1:
            raise ValueError("properties without delimiter")
        key, values = input_[position : position + index], []
        if not key.isupper():
            raise ValueError("property must be in uppercase")
        position = position + index
        while input_[position] == "[":
            position = index = position + 1
            value = []
            while input_[index] != "]":
                if input_[index] == "\t":
                    value.append(" ")
                elif input_[index : index + 2] in ("\\]", "\\\\"):
                    value.append(input_[index + 1])
                    index += 1
                elif input_[index : index + 2] == "\\\n":
                    index += 1
                elif input_[index] != "\\":
                    value.append(input_[index])
                index += 1
            values.append("".join(value))
            position = index + 1
        return {key: values}

    def get_node():
        """
        Extract a node from the current position in the input string.

        Returns:
            SgfTree: The parsed node.

        Raises:
            ValueError: If a node is not properly formatted.
        """
        nonlocal position
        properties = {}
        children = []
        while input_[position] == "(":
            position += 1
        if input_[position] == ")":
            raise ValueError("tree with no nodes")
        position += 1
        while input_[position] not in ("(", ")", ";"):
            properties.update(get_property())
        while position < len(input_) and input_[position] != ")":
            children.append(get_node())
        position += 1
        return SgfTree(properties, children)

    if not input_.startswith("("):
        raise ValueError("tree missing")
    return get_node()
