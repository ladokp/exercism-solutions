def slices(series, length):
    """
    Generate all possible contiguous substrings of a given length from a series.

    Args:
        series (str): The string from which to generate slices.
        length (int): The length of each slice.

    Returns:
        list: A list of substrings of the specified length from the series.

    Raises:
        ValueError: If the series is empty.
        ValueError: If the length is greater than the series length.
        ValueError: If the length is zero or negative.

    Example:
        >>> slices("12345", 2)
        ['12', '23', '34', '45']
    """
    if series == "":
        raise ValueError("series cannot be empty")
    if length > (series_length := len(series)):
        raise ValueError("slice length cannot be greater than series length")
    if length == 0:
        raise ValueError("slice length cannot be zero")
    if length < 0:
        raise ValueError("slice length cannot be negative")

    return [
        series[index : index + length]
        for index in range(series_length)
        if index + length <= series_length
    ]
