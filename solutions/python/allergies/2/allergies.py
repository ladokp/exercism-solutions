class Allergies:
    ingredients = (
        "eggs",
        "peanuts",
        "shellfish",
        "strawberries",
        "tomatoes",
        "chocolate",
        "pollen",
        "cats",
    )

    def __init__(self, score):
        self.score = score

    def allergic_to(self, item):
        return bool(self.score & 2 ** Allergies.ingredients.index(item))

    @property
    def lst(self):
        return [
            ingredient
            for ingredient in Allergies.ingredients
            if self.allergic_to(ingredient)
        ]
