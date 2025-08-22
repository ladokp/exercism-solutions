class Binary
  constructor: (base2_number) ->
      @binary = base2_number.split("").reverse()

  toDecimal: ->
    decimal_number = 0
    for index in [0 .. @binary.length - 1] by 1
        current_digit = parseInt(@binary[index])
        if current_digit not in [0, 1]
            decimal_number = 0
            break
        decimal_number += current_digit * (Math.pow 2, index)
    decimal_number
module.exports = Binary
