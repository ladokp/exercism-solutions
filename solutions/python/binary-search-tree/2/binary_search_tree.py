class TreeNode:
    """
    A node in a binary tree.

    Attributes:
        data: The value stored in the node.
        left: A reference to the left child node (TreeNode).
        right: A reference to the right child node (TreeNode).
    """

    def __init__(self, data, left=None, right=None):
        """
        Initializes a TreeNode with the given data, left child, and right child.

        Args:
            data: The value to be stored in this node.
            left: The left child of this node (default is None).
            right: The right child of this node (default is None).
        """
        self.data = data
        self.left = left
        self.right = right

    def __str__(self):
        """
        Returns a string representation of the TreeNode.

        Returns:
            A string that represents the TreeNode.
        """
        return (
            f"TreeNode(data={self.data}, left={self.left}, right={self.right})"
        )


class BinarySearchTree:
    """
    A binary search tree (BST) data structure.

    This class provides methods to insert nodes, search for values, and retrieve sorted
    data from the tree.

    Attributes:
        root: The root node of the binary search tree (TreeNode).
    """

    def __init__(self, data=None):
        """
        Initializes a BinarySearchTree. If provided, inserts the given data into the tree.

        Args:
            data: An optional iterable of values to be inserted into the tree.
        """
        self.root = None
        if data is not None:
            for node in data:
                self.insert(node)

    def insert(self, data):
        """
        Inserts a new value into the binary search tree.

        Args:
            data: The value to be inserted into the tree.
        """
        if self.root is None:
            self.root = TreeNode(data)
        else:
            self._insert_recursive(data, self.root)

    def _insert_recursive(self, data, node):
        """
        Helper method to recursively insert a new value into the binary search tree.

        Args:
            data: The value to be inserted.
            node: The current node being compared against.
        """
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
        """
        Searches for a value in the binary search tree.

        Args:
            data: The value to search for in the tree.

        Returns:
            The TreeNode containing the data if found; otherwise, None.
        """
        return self._search_recursive(data, self.root)

    def _search_recursive(self, data, node):
        """
        Helper method to recursively search for a value in the binary search tree.

        Args:
            data: The value to search for.
            node: The current node being examined.

        Returns:
            The TreeNode containing the data if found; otherwise, None.
        """
        if node is None or node.data == data:
            return node
        if data < node.data:
            return self._search_recursive(data, node.left)
        return self._search_recursive(data, node.right)

    def data(self):
        """
        Returns the root node of the binary search tree.

        Returns:
            The root TreeNode of the BST.
        """
        return self.root

    def sorted_data(self):
        """
        Retrieves a list of all values in the binary search tree sorted in ascending order.

        Returns:
            A list of values from the tree in sorted order.
        """
        sorted_data = []
        self._inorder_traversal(self.root, sorted_data)
        return sorted_data

    def _inorder_traversal(self, node, sorted_data):
        """
        Helper method to perform an inorder traversal of the binary search tree.

        Args:
            node: The current TreeNode being traversed.
            sorted_data: The list that collects the nodes' values in sorted order.
        """
        if node is not None:
            self._inorder_traversal(node.left, sorted_data)
            sorted_data.append(node.data)
            self._inorder_traversal(node.right, sorted_data)
