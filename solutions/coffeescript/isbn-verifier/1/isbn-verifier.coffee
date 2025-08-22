class IsbnVerifier
  @isValid: (isbn) ->
    digits = isbn.replace /-/g, ''
    return false if digits.length != 10
  
    sum = 0
    for index in [0..9]
      digit = digits[index]
      digit = 10 if index == 9 && digit == 'X'        
      return false if /\D/.test digit      
      sum += digit * (10 - index)
    sum % 11 == 0

module.exports = IsbnVerifier