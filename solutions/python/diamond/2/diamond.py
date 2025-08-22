def rows(letter):
    letters = tuple(
        chr(code_point) for code_point in range(ord("A"), ord(letter) + 1)
    )
    return [
        "".join(
            x if x == y else " " for y in tuple(letters[::-1] + letters[1:])
        )
        for x in tuple(letters[:-1] + letters[::-1])
    ]
