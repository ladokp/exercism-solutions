class Allergies:
    """
    A class to represent a person's allergies based on a score.
    """

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

    def __init__(self, score: int) -> None:
        """
        Initializes the Allergies class with a given score.

        Parameters:
        score (int): The allergy score that determines the allergies.
        """
        self.score = score

    def allergic_to(self, item: str) -> bool:
        """
        Checks if the person is allergic to a specific ingredient.

        Parameters:
        item (str): The ingredient to check against the allergies.

        Returns:
        bool: True if allergic to the ingredient, False otherwise.
        """
        return bool(self.score & 2 ** Allergies.ingredients.index(item))

    @property
    def lst(self) -> list:
        """
        Returns a list of all ingredients the person is allergic to.

        Returns:
        list: A list of allergic ingredients.
        """
        return [
            ingredient
            for ingredient in Allergies.ingredients
            if self.allergic_to(ingredient)
        ]
