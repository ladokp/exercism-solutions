def flatten(iterable):
    flattened_iterable = []
    for item in iterable:
        if isinstance(item, (tuple, list)):
            flattened_iterable += flatten(item)
        elif item is not None:
            flattened_iterable.append(item)
    return flattened_iterable
