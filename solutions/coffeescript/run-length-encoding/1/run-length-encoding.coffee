class RunLengthEncoding

  # Encode the input string using Run-Length Encoding
  @encode: (string) ->
    # Regular expression to match consecutive characters
    consecutiveChars = /([\w\s])\1*/g
    # Replace consecutive characters with their count followed by the character
    string.replace consecutiveChars, (match) ->
      return match.length + match[0] if match.length > 1  # If more than one character, prepend the count
      match[0]  # If only one character, return the character itself

  # Decode the encoded string back to its original form
  @decode: (string) ->
    # Regular expression to match the count followed by a character
    countAndChar = /(\d+)(\w|\s)/g
    # Replace the count and character with the character repeated 'count' times
    string.replace countAndChar, (_, count, char) ->
      char.repeat parseInt(count, 10)  # Repeat the character 'count' times

# Export the RunLengthEncoding class as a module
module.exports = RunLengthEncoding
