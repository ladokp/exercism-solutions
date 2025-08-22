module.exports = class Wordy
  ERROR:
    unknownOperation: "unknown operation",
    syntaxError: "syntax error"
    
  format: /^What is(( -?\d+)|( plus)|( minus)|( multiplied by)|( divided by))*\?$/
  
  constructor: (@question) ->
    
  answer: ->
    throw @ERROR.unknownOperation unless @format.test @question
    tokens = @question[8..-2].replace(/ by/g, '').split(' ')
    result = tokens[0] - 0 || NaN
    for i in [1...tokens.length] by 2
      number = tokens[i + 1] - 0
      switch tokens[i]
        when "plus"       then result += number
        when "minus"      then result -= number
        when "multiplied" then result *= number
        when "divided"    then result /= number
        else throw @ERROR.syntaxError
    if isNaN result
    then throw @ERROR.syntaxError
    else result