class Pangram
  @isPangram: (sentence) ->
    alphabet = 'abcdefghijklmnopqrstuvwxyz'
    lowercase = sentence.toLowerCase()
    for index in [0..alphabet.length-1] by 1
      return false if (lowercase.indexOf(alphabet.charAt(index), 0) == -1)
    return true

module.exports = Pangram