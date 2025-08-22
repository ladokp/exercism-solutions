"""
This module generates verses for the song "I Know an Old Lady Who Swallowed a Fly."
It defines the Animal named tuple and provides functions to generate verses and recite the song.
"""

from typing import NamedTuple


class Animal(NamedTuple):
    """
    A class to represent an animal with a kind and a rhyme.
    """

    kind: str
    rhyme: str


ANIMALS = (
    Animal("fly", ""),
    Animal("spider", "It wriggled and jiggled and tickled inside her."),
    Animal("bird", "How absurd to swallow a bird!"),
    Animal("cat", "Imagine that, to swallow a cat!"),
    Animal("dog", "What a hog, to swallow a dog!"),
    Animal("goat", "Just opened her throat and swallowed a goat!"),
    Animal("cow", "I don't know how she swallowed a cow!"),
    Animal("horse", ""),
)


def verse(nth):
    """
    Generate a single verse of the song for the nth animal.

    Parameters:
        nth (int): The number of the verse to generate.

    Returns:
        List[str]: The lines of the generated verse.
    """
    animal = ANIMALS[nth - 1]
    result = [f"I know an old lady who swallowed a {animal.kind}."]
    if animal.kind != "horse":
        if animal.rhyme:
            result.append(animal.rhyme)
        for other_animal in reversed(ANIMALS[: nth - 1]):
            roll = f"She swallowed the {animal.kind} to catch the {other_animal.kind}"
            if other_animal.kind == "spider":
                roll = f"{roll} that {other_animal.rhyme[3:-1]}"
            roll += "."
            result.append(roll)
            animal = other_animal
        result.append(
            "I don't know why she swallowed the fly. Perhaps she'll die."
        )
    else:
        result.append("She's dead, of course!")
    return result


def recite(start, end):
    """
    Recite verses of the song from start to end.

    Parameters:
        start (int): The starting verse number.
        end (int): The ending verse number.

    Returns:
        List[str]: The lines of the generated verses.
    """
    result = []
    for position in range(start, end + 1):
        result.extend(verse(position))
        if position < end:
            result.append("")
    return result
