class DifferenceOfSquares
  @squareOfSum: (n) ->
    (n * (n + 1) / 2) ** 2

  @sumOfSquares: (n) ->
    n * (n + 1) * (2 * n + 1) / 6

  @differenceOfSquares: (n) ->
    @squareOfSum(n) - @sumOfSquares(n)

module.exports = DifferenceOfSquares
