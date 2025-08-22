"""
This module defines InputCell and ComputeCell classes, 
allowing for reactive programming by managing dependencies and callbacks.
"""

from typing import Callable, List


class InputCell(object):
    """
    Represents a cell with a mutable integer value, notifying registered callbacks upon change.
    """

    def __init__(self, initial_value: int) -> None:
        """
        Initializes an InputCell with an initial integer value.

        :param initial_value: The starting value of the cell.
        """
        self._value = initial_value
        self.callbacks = set()

    @property
    def value(self) -> int:
        """
        Retrieves the current value of the cell.

        :return: Current integer value of the cell.
        """
        return self._value

    @value.setter
    def value(self, new_value: int) -> None:
        """
        Sets a new value for the cell and triggers callbacks if the value changes.

        :param new_value: The new integer value to set.
        """
        self.update_value(new_value)

    def update_value(self, new_value: int) -> None:
        """
        Updates the cell's value if it differs from the current one and triggers callbacks.

        :param new_value: The new integer value to set.
        """
        if self._value != new_value:
            self._value = new_value
            for callback in self.callbacks:
                callback(new_value)

    def add_callback(self, callback: Callable) -> None:
        """
        Registers a callback to be executed when the cell's value changes.

        :param callback: A callable object to register.
        """
        self.callbacks.add(callback)

    def remove_callback(self, callback: Callable) -> None:
        """
        Unregisters a previously added callback.

        :param callback: A callable object to unregister.
        """
        self.callbacks.discard(callback)


class ComputeCell(InputCell):
    """
    Represents a cell whose value is computed based on other input cells.
    """

    def __init__(
        self, inputs: List[InputCell], compute_function: Callable
    ) -> None:
        """
        Initializes a ComputeCell that calculates its value using a function and input cells.

        :param inputs: List of InputCell instances acting as dependencies.
        :param compute_function: Function used to compute the cell's value.
        """
        self.feeds = inputs
        self.process = compute_function
        super().__init__(self.value)
        for feed in self.feeds:
            feed.add_callback(lambda x: self.evaluate())

    @property
    def value(self) -> int:
        """
        Computes and returns the cell's value using the provided function.

        :return: Computed integer value of the cell.
        """
        return self.process([feed.value for feed in self.feeds])

    def evaluate(self) -> None:
        """
        Reevaluates the cell's value and updates it if necessary.
        """
        super().update_value(self.value)
