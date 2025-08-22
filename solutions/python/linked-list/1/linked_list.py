class Node:
    def __init__(self, value, next_=None, previous_=None):
        self._value = value
        self._next = next_
        self._previous = previous_

    def value(self):
        return self._value

    def next(self):
        return self._next

    def previous(self):
        return self._previous


class LinkedList:
    def __init__(self, values=None):
        if values is None:
            values = []
        self._head = None
        self._tail = None
        self._count = 0
        for v in values:
            self.push(v)

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
        if self._head.previous():
            self._head.previous()._next = self._head
        self._count += 1
        if self._count == 1:
            self._tail = self._head

    def unshift(self, value):
        self._tail = Node(value, self._tail, None)
        if self._tail.next():
            self._tail.next()._previous = self._tail
        self._count += 1
        if self._count == 1:
            self._head = self._tail

    def pop(self):
        if self._head is None:
            raise EmptyListException()
        value = self._head.value()
        self._head = self._head.previous()
        if self._head:
            self._head._next = None
        self._count -= 1
        return value

    def shift(self):
        if self._tail is None:
            raise EmptyListException()
        value = self._tail.value()
        self._tail = self._tail.next()
        if self._tail:
            self._tail._previous = None
        self._count -= 1
        return value

    def delete(self, value):
        for node in self:
            if node.value() == value:
                if node.next():
                    node.next()._previous = node.previous()
                else:
                    self._head = node.previous()
                if node.previous():
                    node.previous()._next = node.next()
                else:
                    self._tail = node.next()
                self._count -= 1
                break
        else:
            raise ValueNotFoundException()

    def __iter__(self):
        node = self._tail
        while node is not None:
            yield node
            node = node.next()


class EmptyListException(IndexError):
    def __init__(self, message="The list is empty."):
        super().__init__(message)


class ValueNotFoundException(ValueError):
    def __init__(self, message="Value not found"):
        super().__init__(message)
