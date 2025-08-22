class SgfTree:
    def __init__(self, properties=None, children=None):
        self.properties, self.children = properties or {}, children or []

    def __eq__(self, other):
        return (
            self.properties == other.properties
            and self.children == other.children
        )


def parse(input_):
    position = 1

    def get_property():
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
