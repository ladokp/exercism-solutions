class Allergies:
    ingredients = {
        "eggs": 1,
        "peanuts": 2,
        "shellfish": 4,
        "strawberries": 8,
        "tomatoes": 16,
        "chocolate": 32,
        "pollen": 64,
        "cats": 128,
    }

    def __init__(self, score):
        self.score = score

    def allergic_to(self, item):
        return bool(self.score & Allergies.ingredients.get(item))

    @property
    def lst(self):
        return [
            ingredient
            for ingredient in Allergies.ingredients
            if self.allergic_to(ingredient)
        ]
