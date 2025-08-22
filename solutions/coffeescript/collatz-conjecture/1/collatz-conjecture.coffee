class CollatzConjecture
  @steps: (number) ->
    throw new Error('Only positive integers are allowed') if number < 1
      
    steps = 0
    while number > 1
      if number % 2 == 0 then number /= 2 else number = 3 * number + 1
      steps += 1
    steps

module.exports = CollatzConjecture
