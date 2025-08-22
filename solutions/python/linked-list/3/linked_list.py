class Node:
    def __init__(self, value_, next_, previous_):
        self.value = value_
        self.next = next_
        self.previous = previous_

    @property
    def value(self):
        return self._value

    @value.setter
    def value(self, value):
        self._value = value

    @property
    def next(self):
        return self._next

    @next.setter
    def next(self, node):
        self._next = node
        if node is not None:
            node.set_previous_only(self)

    def set_next_only(self, node):
        self._next = node

    @property
    def previous(self):
        return self._previous

    @previous.setter
    def previous(self, node):
        self._previous = node
        if node is not None:
            node.set_next_only(self)

    def set_previous_only(self, node):
        self._previous = node

    def delete(self):
        if self._next is not None:
            self._next.previous = self._previous
            self._previous = None
        if self._previous is not None:
            self._previous.next = self._next
            self._next = None


class LinkedList:
    def __init__(self, values=None):
        if values is None:
            values = []
        self._head = self._tail = None
        self._count = 0
        for value in values:
            self.push(value)

    def __len__(self):
        return self._count

    def head(self):
        if self._head is None:
            raise EmptyListException()
        return self._head

    def tail(self):
        if self._tail is None:
            raise EmptyListException()
        return self._tail

    def push(self, value):
        self._head = Node(value, None, self._head)
        self._count += 1
        if self._count == 1:
            self._tail = self._head

    def unshift(self, value):
        self._tail = Node(value, self._tail, None)
        self._count += 1
        if self._count == 1:
            self._head = self._tail

    def pop(self):
        if self._head is None:
            raise EmptyListException()
        node = self._head
        self._head = node.previous
        self._count -= 1
        node.delete()
        return node.value

    def shift(self):
        if self._tail is None:
            raise EmptyListException()
        node = self._tail
        self._tail = node.next
        self._count -= 1
        node.delete()
        return node.value

    def delete(self, value):
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
        node = self._tail
        while node is not None:
            yield node
            node = node.next


class EmptyListException(IndexError):
    def __init__(self, message="The list is empty."):
        super().__init__(message)


class ValueNotFoundException(ValueError):
    def __init__(self, message="Value not found"):
        super().__init__(message)
