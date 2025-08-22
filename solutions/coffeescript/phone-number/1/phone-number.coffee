class PhoneNumber
   constructor: (@number) ->

   clean: ->
      throw "letters not permitted" if /[a-zA-Z]/.exec @number
      throw "punctuations not permitted" if /[^-+.a-zA-Z0-9()\s]/.exec @number

      cleaned = @number.split('').reduceRight (accumulator, digit) ->
         if '0' <= digit <= '9'
            digit + accumulator
         else
            accumulator
      , ''

      throw "must not be fewer than 10 digits" if cleaned.length < 10
      throw "must not be more than 11 digits" if cleaned.length > 11
      throw "area code cannot start with zero" if cleaned[cleaned.length - 10] == '0'
      throw "area code cannot start with one" if cleaned[cleaned.length - 10] == '1'
      throw "exchange code cannot start with zero" if cleaned[cleaned.length - 7] == '0'
      throw "exchange code cannot start with one" if cleaned[cleaned.length - 7] == '1'

      if cleaned.length == 11
         if cleaned[0] == '1'
            return cleaned.substr -10
         else
            throw "11 digits must start with 1"

      cleaned

module.exports = PhoneNumber