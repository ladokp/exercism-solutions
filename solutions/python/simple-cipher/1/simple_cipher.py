import random
import string
from itertools import cycle

class Cipher:
    def __init__(self, key=None):
        self.letters = string.ascii_lowercase
        if key is None:
            self.key = "".join(random.choice(self.letters) for _ in range(26))
        else:
            self.key = key

    def encode(self, text, /, shifting_mode=1):
        encoded = []
        for character_1, character_2 in zip(text, cycle(self.key)):
            encoded.append(self.letters[(ord(character_1) % 97 + (ord(character_2) % 97) * shifting_mode) % 26])
        return "".join(encoded)

    def decode(self, text):
        return self.encode(text, shifting_mode=-1)
