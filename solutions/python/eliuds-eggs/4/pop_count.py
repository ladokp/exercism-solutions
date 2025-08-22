def egg_count(display_value: int) -> int:
    available_eggs = 0
    while display_value > 0:
        available_eggs += display_value & 1
        display_value = display_value // 2
    return available_eggs
