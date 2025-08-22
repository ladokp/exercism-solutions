"""Functions to manage a user's shopping cart items."""


def add_item(current_cart, items_to_add):
    """Add given items to the user's shopping cart.

    This function updates the current shopping cart by adding each item
    from the 'items_to_add' iterable. If an item already exists in the
    cart, its quantity is incremented by one.

    :param current_cart: dict - The current state of the user's shopping cart.
    :param items_to_add: iterable - A collection of items to be added to the cart.
    :return: dict - The updated shopping cart with new items added.
    """

    for item in items_to_add:
        current_cart[item] = current_cart.setdefault(item, 0) + 1
    return current_cart


def read_notes(notes):
    """Generate a shopping cart dictionary from an iterable of notes.

    Each entry in the 'notes' iterable represents an item to be added
    to the cart with an initial quantity of one.

    :param notes: iterable - List of items to be added to the cart.
    :return: dict - A shopping cart dictionary initialized with items from notes.
    """

    return dict.fromkeys(notes, 1)


def update_recipes(ideas, recipe_updates):
    """Apply updates to the recipe ideas dictionary.

    This function takes an existing 'ideas' dictionary containing
    recipe ideas and updates it with the key-value pairs from the
    'recipe_updates' dictionary.

    :param ideas: dict - Current "recipe ideas" dictionary.
    :param recipe_updates: dict - Updates to be applied to the ideas dictionary.
    :return: dict - Updated "recipe ideas" dictionary after applying changes.
    """

    ideas.update(recipe_updates)
    return ideas


def sort_entries(cart):
    """Sort a user's shopping cart in alphabetical order.

    Sorts the input shopping cart dictionary by item name to facilitate
    easier navigation and retrieval.

    :param cart: dict - A user's shopping cart dictionary to be sorted.
    :return: dict - Shopping cart sorted alphabetically by item name.
    """

    return dict(sorted(cart.items()))


def send_to_store(cart, isle_mapping):
    """Prepare a user's order with aisle and refrigeration details for the store.

    Combines shopping cart items with corresponding aisle and refrigeration
    information from 'isle_mapping'. This helps in organizing items for
    efficient in-store processing and fulfillment.

    :param cart: dict - User's shopping cart dictionary with item quantities.
    :param isle_mapping: dict - Mapping of each item to its aisle and refrigeration details.
    :return: dict - Prepared dictionary with combined order and location data, sorted in reverse order.
    """

    fulfillment_dictionary = {}
    for item, quantity in cart.items():
        fulfillment_dictionary[item] = [quantity, *isle_mapping[item]]
    return dict(sorted(fulfillment_dictionary.items(), reverse=True))


def update_store_inventory(fulfillment_cart, store_inventory):
    """Update inventory levels in the store with a user's fulfillment cart.

    Reduces the store's available inventory based on the quantities specified
    in the 'fulfillment_cart'. If an item's inventory level reaches zero, it
    is marked as "Out of Stock".

    :param fulfillment_cart: dict - Fulfillment cart specifying items and quantities sent to the store.
    :param store_inventory: dict - Current inventory levels at the store.
    :return: dict - The store's updated inventory after processing the fulfillment cart.
    """

    for item, item_properties in fulfillment_cart.items():
        store_inventory[item][0] -= item_properties[0]
        if not store_inventory[item][0]:
            store_inventory[item][0] = "Out of Stock"
    return store_inventory
