"""
This exercise stub and the test suite contain several enumerated constants.

Since Python 2 does not have the enum module, the idiomatic way to write
enumerated constants has traditionally been a NAME assigned to an arbitrary,
but unique value. An integer is traditionally used because it’s memory
efficient.
It is a common practice to export both constants and functions that work with
those constants (ex. the constants in the os, subprocess and re modules).

You can learn more here: https://en.wikipedia.org/wiki/Enumerated_type
"""

# Possible sublist categories.
# Change the values as you see fit.
SUBLIST = 1
SUPERLIST = 2
EQUAL = 3
UNEQUAL = 4


def sublist(list_one, list_two):
    if list_one == list_two:
        return EQUAL
    elif len(list_one) > len(list_two):
        bigger = list_one
        smaller = list_two
    else:
        bigger = list_two
        smaller = list_one
    for index in range(len(bigger)):
        if bigger[index : index + len(smaller)] == smaller and bigger == list_one:
            return SUPERLIST
        elif bigger[index : index + len(smaller)] == smaller and bigger == list_two:
            return SUBLIST
    return UNEQUAL
