allYourBase = ({inputBase, outputBase, digits}) ->
  throw new Error "input base must be >= 2" unless inputBase >= 2
  throw new Error "output base must be >= 2" unless outputBase >= 2
  any_digits_invalid = digits.some ((digit) => digit < 0 or digit >= inputBase)
  throw new Error "all digits must satisfy 0 <= d < input base" if any_digits_invalid

  decimal_value = digits.reduce ((decimal_value, digit) => decimal_value * inputBase + digit), 0

  if decimal_value is 0
    [0]
  else
    outputDigits = []
    while decimal_value > 0
      outputDigits.unshift decimal_value % outputBase
      decimal_value //= outputBase
    outputDigits


module.exports = allYourBase