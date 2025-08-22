class Etl
  # Static method to transform the legacy data structure
  @transform: (legacy) ->
    results = {}  # Initialize an empty object to store the transformed results
    # Iterate over each score and its corresponding letters in the legacy data
    for score, letters of legacy
      # Iterate over each letter in the letters array
      for letter in letters
        # Convert the letter to lowercase and set its value to the score (converted to a number)
        results[letter.toLowerCase()] = +score
    
    results  # Return the transformed results

# Export the Etl class as a module
module.exports = Etl
