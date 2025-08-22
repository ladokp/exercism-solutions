def commands(binary_str):
    number = int(binary_str, 2)
    handshake = [
        action
        for action, bit in (
            ("wink", 1),
            ("double blink", 2),
            ("close your eyes", 4),
            ("jump", 8),
        )
        if number & bit
    ]
    return handshake[::-1] if number & 16 else handshake
