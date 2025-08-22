def egg_count(display_value):
    bits = 0
    while display_value:
        bits += 1
        display_value &= display_value - 1
    return bits
