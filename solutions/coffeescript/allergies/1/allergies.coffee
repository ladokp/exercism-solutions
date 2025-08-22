ALLERGENS = [
  "eggs"
  "peanuts"
  "shellfish"
  "strawberries"
  "tomatoes"
  "chocolate"
  "pollen"
  "cats"
]

class Allergies
  constructor: (@score) ->
  allergicTo: (food) -> @list().some (allergen) -> allergen == food
  list: () -> ALLERGENS.filter (_, index) => @score & (1 << index)

module.exports = Allergies