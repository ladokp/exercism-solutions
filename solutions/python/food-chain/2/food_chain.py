from typing import List, NamedTuple


class Animal(NamedTuple):
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
    result = []
    for nth in range(start, end + 1):
        result.extend(verse(nth))
        if nth < end:
            result.append("")
    return result
