class Node:
    def __init__(self, value_: any, next_: "Node", previous_: "Node"):
        """Initialize a new node with a value, next node, and previous node.

        Args:
            value_ (any): The value held by the node.
            next_ (Node): The next node in the list.
            previous_ (Node): The previous node in the list.
        """
        self.value = value_
        self.next = next_
        self.previous = previous_

    @property
    def value(self) -> any:
        """Get the value of the node."""
        return self._value

    @value.setter
    def value(self, value: any) -> None:
        """Set the value of the node.

        Args:
            value (any): The new value to set.
        """
        self._value = value

    @property
    def next(self) -> "Node":
        """Get the next node."""
        return self._next

    @next.setter
    def next(self, node: "Node") -> None:
        """Set the next node.

        Args:
            node (Node): The node to set as next.
        """
        self._next = node
        if node is not None:
            node.set_previous_only(self)

    def set_next_only(self, node: "Node") -> None:
        """Set the next node without affecting previous links.

        Args:
            node (Node): The node to set as next.
        """
        self._next = node

    @property
    def previous(self) -> "Node":
        """Get the previous node."""
        return self._previous

    @previous.setter
    def previous(self, node: "Node") -> None:
        """Set the previous node.

        Args:
            node (Node): The node to set as previous.
        """
        self._previous = node
        if node is not None:
            node.set_next_only(self)

    def set_previous_only(self, node: "Node") -> None:
        """Set the previous node without affecting next links.

        Args:
            node (Node): The node to set as previous.
        """
        self._previous = node

    def delete(self) -> None:
        """Delete this node from the list."""
        if self._next is not None:
            self._next.previous = self._previous
            self._previous = None
        if self._previous is not None:
            self._previous.next = self._next
            self._next = None


class LinkedList:
    def __init__(self, values=None):
        """Initialize the linked list with an optional list of values.

        Args:
            values (list, optional): Initial values to add to the list. Defaults to None.
        """
        if values is None:
            values = []
        self._head = self._tail = None
        self._count = 0
        for value in values:
            self.push(value)

    def __len__(self) -> int:
        """Return the number of items in the list."""
        return self._count

    def head(self) -> Node:
        """Get the head node of the list.

        Raises:
            EmptyListException: If the list is empty.
        """
        if self._head is None:
            raise EmptyListException()
        return self._head

    def tail(self) -> Node:
        """Get the tail node of the list.

        Raises:
            EmptyListException: If the list is empty.
        """
        if self._tail is None:
            raise EmptyListException()
        return self._tail

    def push(self, value: any) -> None:
        """Add a value to the front of the list.

        Args:
            value (any): The value to add.
        """
        self._head = Node(value, None, self._head)
        self._count += 1
        if self._count == 1:
            self._tail = self._head

    def unshift(self, value: any) -> None:
        """Add a value to the end of the list.

        Args:
            value (any): The value to add.
        """
        self._tail = Node(value, self._tail, None)
        self._count += 1
        if self._count == 1:
            self._head = self._tail

    def pop(self) -> any:
        """Remove and return the value from the front of the list.

        Raises:
            EmptyListException: If the list is empty.
        """
        if self._head is None:
            raise EmptyListException()
        node = self._head
        self._head = node.previous
        self._count -= 1
        node.delete()
        return node.value

    def shift(self) -> any:
        """Remove and return the value from the end of the list.

        Raises:
            EmptyListException: If the list is empty.
        """
        if self._tail is None:
            raise EmptyListException()
        node = self._tail
        self._tail = node.next
        self._count -= 1
        node.delete()
        return node.value

    def delete(self, value: any) -> None:
        """Delete the first occurrence of a value from the list.

        Args:
            value (any): The value to remove.

        Raises:
            ValueNotFoundException: If the value is not found.
        """
        for node in self:
            if node.value == value:
                node.delete()
                if node is self._head:
                    self._head = node.previous
                if node is self._tail:
                    self._tail = node.next
                self._count -= 1
                return
        raise ValueNotFoundException()

    def __iter__(self):
        """Iterate over the nodes in the list."""
        node = self._tail
        while node is not None:
            yield node
            node = node.next


class EmptyListException(IndexError):
    def __init__(self, message="The list is empty."):
        """Initialize the exception with a message.

        Args:
            message (str): The exception message.
        """
        super().__init__(message)


class ValueNotFoundException(ValueError):
    def __init__(self, message="Value not found"):
        """Initialize the exception with a message.

        Args:
            message (str): The exception message.
        """
        super().__init__(message)
