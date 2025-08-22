class Anagram
  constructor: (word) ->
    @word = word.toLowerCase()
    @sortedWord = @sorted(@word)

  match: (targets) ->
    result = (item for item in targets when (item.toLowerCase() != @word) && (@sortedWord ==     @sorted(item.toLowerCase())) )

  sorted: (word) ->
    word.split('').sort().join('') 

module.exports = Anagram
