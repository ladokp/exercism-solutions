class MatchingBrackets
  @isPaired: (value) ->
    stack = []
    openingBrackets = ['[', '{', '(']
    closingBrackets = [']', '}', ')']
    matchingBrackets = {
      ']': '['
      '}': '{'
      ')': '('
    }

    for char in value
      if char in openingBrackets
        stack.push char
      else if char in closingBrackets
        return false if stack.length is 0 or stack.pop() != matchingBrackets[char]

    stack.length == 0

module.exports = MatchingBrackets
