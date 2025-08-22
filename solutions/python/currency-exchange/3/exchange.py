"""
A simple currency calculator that allows for exchanging money between different currencies,
calculating change, and determining the value and number of bills based on given denominations.
"""


def exchange_money(budget, exchange_rate):
    """
    Calculate the amount of foreign currency received by exchanging a specified budget.

    :param budget: float - Amount of money you are planning to exchange.
    :param exchange_rate: float - Unit value of the foreign currency.
    :return: float - The amount of foreign currency received from the exchange.
    """
    return budget / exchange_rate


def get_change(budget, exchanging_value):
    """
    Calculate the remaining amount of currency after a specified exchange.

    :param budget: float - Total amount of money you have.
    :param exchanging_value: float - Amount of your money you want to exchange now.
    :return: float - Remaining amount of your starting currency after the exchange.
    """
    return budget - exchanging_value


def get_value_of_bills(denomination, number_of_bills):
    """
    Calculate the total value of a specified number of currency bills.

    :param denomination: int - The value of a single bill.
    :param number_of_bills: int - Number of bills received.
    :return: int - Total value of the currency bills you possess.
    """
    return number_of_bills * denomination


def get_number_of_bills(budget, denomination):
    """
    Determine the total number of bills that can be obtained from a specified budget at a given denomination.

    :param budget: float - Amount of money you plan to exchange.
    :param denomination: int - Value of a single bill.
    :return: int - Total number of bills that can be obtained from the exchange.
    """
    return int(budget / denomination)


def get_leftover_of_bills(budget, denomination):
    """
    Calculate the leftover amount that cannot be exchanged given the current bill denomination.

    :param budget: float - Amount of money you are planning to exchange.
    :param denomination: int - Value of a single bill.
    :return: float - The remainder of money that cannot be exchanged into bills.
    """
    return budget % denomination


def exchangeable_value(budget, exchange_rate, spread, denomination):
    """
    Calculate the maximum exchangeable value after considering an exchange fee and denomination limits.

    :param budget: float - Amount of money you are planning to exchange.
    :param exchange_rate: float - Unit value of the foreign currency.
    :param spread: int - Percentage taken as an exchange fee.
    :param denomination: int - Value of a single bill.
    :return: int - Maximum value you can receive after the exchange.
    """
    exchange_rate = exchange_rate * (1 + spread / 100)
    exchanged_money = exchange_money(budget, exchange_rate)
    number_of_bills = get_number_of_bills(exchanged_money, denomination)

    return get_value_of_bills(denomination, number_of_bills)
