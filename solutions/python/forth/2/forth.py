"""
Module for stack-based operations and evaluation.
"""

import inspect
from itertools import chain
import string

OPERATIONS = {
    "+": lambda x, y: [y + x],
    "-": lambda x, y: [y - x],
    "*": lambda x, y: [y * x],
    "/": lambda x, y: [y // x],
    "dup": lambda x: [x, x],
    "drop": lambda _: [],
    "swap": lambda x, y: [x, y],
    "over": lambda x, y: [y, x, y],
}


class StackUnderflowError(Exception):
    """
    Exception raised for stack underflow errors.
    """

    def __init__(self):
        super().__init__("Insufficient number of items in stack")


def is_number(elem):
    """
    Check if the given element is a number.

    Args:
        elem (str): The element to check.

    Returns:
        bool: True if the element is a number, False otherwise.
    """
    return elem and (
        set(elem) < set(string.digits)
        or (elem[0] == "-" and is_number(elem[1:]))
    )


def apply(stack, elem):
    """
    Apply an operation or push a number onto the stack.

    Args:
        stack (list): The current stack.
        elem (str): The element to apply or push.

    Raises:
        ValueError: If the operation is undefined.
        StackUnderflowError: If there are not enough items on the stack for the operation.
    """
    if is_number(elem):
        stack.append(int(elem))
    elif elem in OPERATIONS:
        operation = OPERATIONS[elem]
        count = len(inspect.signature(operation).parameters)
        print(elem, count)
        if len(stack) < count:
            raise StackUnderflowError
        stack.extend(operation(*(stack.pop() for x in range(count))))
    else:
        raise ValueError("undefined operation")


def substitute(custom, elems):
    """
    Substitute custom operations in the elements list.

    Args:
        custom (dict): Custom operations dictionary.
        elems (list): List of elements to perform substitution on.

    Returns:
        list: The list with substituted custom operations.
    """
    return list(chain(*(custom[x] if x in custom else [x] for x in elems)))


def evaluate(input_data):
    """
    Evaluate the input data using stack-based operations.

    Args:
        input_data (iterable): Iterable containing lines of input data.

    Returns:
        list: The resulting stack after evaluation.

    Raises:
        ValueError: If an illegal operation is encountered.
        ZeroDivisionError: If a division by zero occurs.
        StackUnderflowError: If there are not enough items on the stack for an operation.
    """
    stack, custom = [], {}
    try:
        for line in input_data:
            elements = line.lower().split()
            if elements[0] == ":":
                if is_number(operation := elements[1]):
                    raise ValueError("illegal operation")
                custom[operation] = substitute(custom, elements[2:-1])
            else:
                for element in substitute(custom, elements):
                    apply(stack, element)
        return stack
    except ZeroDivisionError as error:
        raise ZeroDivisionError("divide by zero") from error
