class SumOfMultiples
  @sum: (factors, limit) ->
    multiples = []
    for index in [1..limit - 1]
      for factor in factors
        if index % factor is 0
          multiples.push index
          break
    multiples.reduce (a, b) ->
      a + b
    , 0

module.exports = SumOfMultiples
