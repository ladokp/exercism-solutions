def convert(number):
    return "".join(f"Pl{character}ng" 
                   for condition, character in ((3, "i"), (5, "a"), (7, "o"))
                   if not number % condition) or str(number)
