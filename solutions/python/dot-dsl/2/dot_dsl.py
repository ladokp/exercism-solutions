NODE, EDGE, ATTR = range(3)


class Node:
    def __init__(self, name, attrs):
        self.name = name
        self.attrs = attrs

    def __eq__(self, other):
        return self.name == other.name and self.attrs == other.attrs


class Edge:
    def __init__(self, src, dst, attrs):
        self.src = src
        self.dst = dst
        self.attrs = attrs

    def __eq__(self, other):
        return (
            self.src == other.src
            and self.dst == other.dst
            and self.attrs == other.attrs
        )


class Graph:
    def __init__(self, data=None):
        self.nodes, self.edges, self.attrs = [], [], {}
        if data is None:
            return
        self.build_graph(data)

    def build_graph(self, data=None) -> None:
        if not hasattr(data, "__getitem__") or isinstance(data, str):
            raise TypeError("Graph data malformed")
        for element in data:
            self._is_good_input(element)
            if element[0] == NODE:
                self.nodes += [Node(element[1], element[2])]
            elif element[0] == EDGE:
                self.edges += [Edge(element[1], element[2], element[3])]
            elif element[0] == ATTR:
                self.attrs[element[1]] = element[2]

    @staticmethod
    def _is_good_input(data) -> bool:
        if len(data) < 3 or len(data) > 4:
            raise TypeError("Graph item incomplete")
        if data[0] not in range(3):
            raise ValueError("Unknown item")
        if data[0] == NODE and len(data) != 3:
            raise ValueError("Node is malformed")
        if data[0] == EDGE and len(data) != 4:
            raise ValueError("Edge is malformed")
        if data[0] == ATTR and len(data) != 3:
            raise ValueError("Attribute is malformed")
        return True
