BUNDLE_PRICE = {1: 800, 2: 1520, 3: 2160, 4: 2560, 5: 3000}


def price(book_quantities):
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


def total(basket):
    return price(basket.count(n) for n in range(1, 6) if n in basket)
