class PrimeFactors
  @factors: (value) ->
    divisor = 2
    factors = []
    while value > 1
      while value % divisor == 0
        factors.push divisor
        value /= divisor
      divisor += 1
    factors

module.exports = PrimeFactors