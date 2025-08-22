class ArmstrongNumbers
  @isArmstrongNumber: (number) ->
    digits = number.toString().split ''
    number == digits.reduce (sum, digit) ->
        sum + Math.pow digit, digits.length
    , 0

module.exports = ArmstrongNumbers
