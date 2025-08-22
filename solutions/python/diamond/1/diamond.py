def rows(letter):
    letters = [
        chr(code_point) for code_point in range(ord("A"), ord(letter) + 1)
    ]
    alphabet = letters[:-1] + letters[::-1]
    diamond_line = letters[::-1] + letters[1:]
    return [
        "".join(x if x == y else " " for y in diamond_line) for x in alphabet
    ]
