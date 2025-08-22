"""
This module provides classes to represent a graph consisting of nodes, edges, and attributes.
"""

NODE, EDGE, ATTR = range(3)


class Node:
    """
    Represents a node in the graph.
    """

    def __init__(self, name, attrs):
        """
        Initializes a new node.

        :param name: The name of the node.
        :param attrs: The attributes of the node.
        """
        self.name = name
        self.attrs = attrs

    def __eq__(self, other):
        """
        Checks equality between two nodes.

        :param other: The other node to compare.
        :return: True if nodes are equal, else False.
        """
        return self.name == other.name and self.attrs == other.attrs


class Edge:
    """
    Represents an edge in the graph.
    """

    def __init__(self, src, dst, attrs):
        """
        Initializes a new edge.

        :param src: The source node of the edge.
        :param dst: The destination node of the edge.
        :param attrs: The attributes of the edge.
        """
        self.src = src
        self.dst = dst
        self.attrs = attrs

    def __eq__(self, other):
        """
        Checks equality between two edges.

        :param other: The other edge to compare.
        :return: True if edges are equal, else False.
        """
        return (
            self.src == other.src
            and self.dst == other.dst
            and self.attrs == other.attrs
        )


class Graph:
    """
    Represents a graph consisting of nodes, edges, and attributes.
    """

    def __init__(self, data=None):
        """
        Initializes a new graph.

        :param data: Optional initial data to build the graph.
        """
        self.nodes, self.edges, self.attrs = [], [], {}
        if data is None:
            return
        self.build_graph(data)

    def build_graph(self, data=None) -> None:
        """
        Builds the graph from the given data.

        :param data: The data to build the graph from.
        :raises TypeError: If the data is malformed.
        """
        if not hasattr(data, "__getitem__") or isinstance(data, str):
            raise TypeError("Graph data malformed")
        for element in data:
            self._is_good_input(element)
            if (first := element[0]) == NODE:
                self.nodes += [Node(element[1], element[2])]
            elif first == EDGE:
                self.edges += [Edge(element[1], element[2], element[3])]
            elif first == ATTR:
                self.attrs[element[1]] = element[2]

    @staticmethod
    def _is_good_input(data) -> bool:
        """
        Validates the input data for building the graph.

        :param data: The data to validate.
        :raises TypeError: If the item is incomplete.
        :raises ValueError: If the item type or length is incorrect.
        :return: True if the data is valid.
        """
        if (number_of_elements := len(data)) < 3 or number_of_elements > 4:
            raise TypeError("Graph item incomplete")
        if (first_element := data[0]) not in range(3):
            raise ValueError("Unknown item")
        if first_element == NODE and number_of_elements != 3:
            raise ValueError("Node is malformed")
        if first_element == EDGE and number_of_elements != 4:
            raise ValueError("Edge is malformed")
        if first_element == ATTR and number_of_elements != 3:
            raise ValueError("Attribute is malformed")
        return True
