class PerfectNumbers
  @classify: (number) ->
    throw new Error 'Classification is only possible for positive integers.' if number <= 0
    
    sum = 0
    for index in [1...number]
      if number % index == 0
        sum += index
    
    if sum < number
      'deficient'
    else if sum > number
      'abundant'
    else
      'perfect'

module.exports = PerfectNumbers
