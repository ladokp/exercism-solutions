def egg_count(display_value: int) -> int:
    available_eggs = 0
    while display_value > 0:
        available_eggs += 1 if display_value % 2 else 0
        display_value //= 2
    return available_eggs
