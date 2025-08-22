class Record:
    def __init__(self, record_id, parent_id):
        self.record_id = record_id
        self.parent_id = parent_id


class Node:
    def __init__(self, node_id):
        self.node_id = node_id
        self.children = []


def BuildTree(records):
    loop_counter = 0
    records.sort(key=lambda x: x.record_id)

    if not records:
        return None
    if records[-1].record_id != len(records) - 1:
        raise ValueError("Record id is invalid or out of order.")
    nodes = {}
    for record in records:
        loop_counter += 1
        if record.record_id == record.parent_id and record.record_id != 0:
            raise ValueError(
                "Only root should have equal record and parent id."
            )
        if record.record_id < record.parent_id:
            raise ValueError(
                "Node parent_id should be smaller than it's record_id."
            )
        node = Node(record.record_id)
        nodes[record.record_id] = node
        if record.record_id == 0:
            continue

        parent = nodes[record.parent_id]
        parent.children.append(node)
    return nodes[0]
