import secrets
from string import ascii_lowercase
from itertools import cycle

class Cipher:
    def __init__(self, key=None):
        if key is None:
            self.key = "".join(secrets.choice(ascii_lowercase) for _ in range(100))
        else:
            self.key = key

    def encode(self, text, /, shifting_mode=1):
        encoded = []
        for character_1, character_2 in zip(text, cycle(self.key)):
            encoded.append(ascii_lowercase[(ord(character_1) % 97 + (ord(character_2) % 97) * shifting_mode) % 26])
        return "".join(encoded)

    def decode(self, text):
        return self.encode(text, shifting_mode=-1)
