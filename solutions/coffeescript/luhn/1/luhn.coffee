class Luhn
  constructor: (input) ->
        @number_to_check = input.split(" ").join("").split("").reverse()

  valid: ->
    result = false
    sum = 0
    if @number_to_check.length <= 1
            return false
    for index in [0 .. @number_to_check.length-1]
        current_number = parseInt(@number_to_check[index])
        if isNaN current_number
            return false
        
        if (index + 1) % 2 == 1
            sum += current_number
        else
            current_number *= 2
            if current_number > 9
                current_number -= 9
            sum += current_number
    sum % 10 == 0
        

module.exports = Luhn
