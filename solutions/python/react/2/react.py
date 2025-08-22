from typing import Callable, List


class InputCell(object):
    def __init__(self, initial_value: int) -> None:
        self._value = initial_value
        self.callbacks = set()

    @property
    def value(self) -> int:
        return self._value

    @value.setter
    def value(self, new_value: int) -> None:
        self.update_value(new_value)

    def update_value(self, new_value: int) -> None:
        if self._value != new_value:
            self._value = new_value
            for callback in self.callbacks:
                callback(new_value)

    def add_callback(self, callback: Callable) -> None:
        self.callbacks.add(callback)

    def remove_callback(self, callback: Callable) -> None:
        self.callbacks.discard(callback)


class ComputeCell(InputCell):
    def __init__(
        self, inputs: List[InputCell], compute_function: Callable
    ) -> None:
        self.feeds = inputs
        self.process = compute_function
        super().__init__(self.value)
        for feed in self.feeds:
            feed.add_callback(lambda x: self.evaluate())

    @property
    def value(self) -> int:
        return self.process([feed.value for feed in self.feeds])

    def evaluate(self) -> None:
        super().update_value(self.value)
