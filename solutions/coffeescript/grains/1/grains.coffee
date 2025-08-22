class Grains
  square: (number) -> 
    throw "square must be between 1 and 64" if number < 1 || number > 64
    Math.pow(2, number - 1)

  total: () ->
    Math.pow(2, 64) - 1

module.exports = Grains
