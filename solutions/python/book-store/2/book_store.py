BUNDLE_PRICE: dict[int, int] = {1: 800, 2: 1520, 3: 2160, 4: 2560, 5: 3000}


def price(book_quantities: list[int]) -> int:
    """Calculate the minimum price for the given quantities of books.

    Args:
        book_quantities (list[int]): A list of integers representing the quantities of books.

    Returns:
        int: The minimum price for the given quantities of books.
    """
    book_quantities = sorted(
        quantity for quantity in book_quantities if quantity != 0
    )
    if not book_quantities:
        return 0
    book_quantities.reverse()
    return min(
        price(
            [quantity - 1 for quantity in book_quantities[:n]]
            + book_quantities[n:]
        )
        + BUNDLE_PRICE[n]
        for n in range(1, len(book_quantities) + 1)
    )


def total(basket: list[int]) -> int:
    """Calculate the total price for the given basket of books.

    Args:
        basket (list[int]): A list of integers representing the books in the basket.

    Returns:
        int: The total price for the given basket of books.
    """
    return price(basket.count(n) for n in range(1, 6) if n in basket)
