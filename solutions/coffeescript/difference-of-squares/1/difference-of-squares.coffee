class DifferenceOfSquares
  @squareOfSum: (number) -> (number * (number + 1) / 2) ** 2

  @sumOfSquares: (number) -> number * (number + 1) * (2 * number + 1) / 6

  @differenceOfSquares: (number) -> @squareOfSum(number) - @sumOfSquares(number)

module.exports = DifferenceOfSquares
