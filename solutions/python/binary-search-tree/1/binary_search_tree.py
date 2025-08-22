class TreeNode:
    def __init__(self, data, left=None, right=None):
        self.data = data
        self.left = left
        self.right = right

    def __str__(self):
        return f'TreeNode(data={self.data}, left={self.left}, right={self.right})'


class BinarySearchTree:
    def __init__(self, data=None):
        self.root = None
        if data is not None:
            for node in data:
                self.insert(node)

    def insert(self, data):
        if self.root is None:
            self.root = TreeNode(data)
        else:
            self._insert_recursive(data, self.root)

    def _insert_recursive(self, data, node):
        if data <= node.data:
            if node.left is None:
                node.left = TreeNode(data)
            else:
                self._insert_recursive(data, node.left)
        elif data > node.data:
            if node.right is None:
                node.right = TreeNode(data)
            else:
                self._insert_recursive(data, node.right)

    def search(self, data):
        return self._search_recursive(data, self.root)

    def _search_recursive(self, data, node):
        if node is None or node.data == data:
            return node
        if data < node.data:
            return self._search_recursive(data, node.left)
        return self._search_recursive(data, node.right)

    def data(self):
        return self.root

    def sorted_data(self):
        sorted_data = []
        self._inorder_traversal(self.root, sorted_data)
        return sorted_data

    def _inorder_traversal(self, node, sorted_data):
        if node is not None:
            self._inorder_traversal(node.left, sorted_data)
            sorted_data.append(node.data)
            self._inorder_traversal(node.right, sorted_data)
