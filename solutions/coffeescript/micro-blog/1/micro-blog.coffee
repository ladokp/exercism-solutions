class Microblog
  @truncate: (phrase) ->
    Array
      .from phrase    # Convert the string 'phrase' into an array of characters
      .slice 0, 5     # Extract the first 5 characters from the array
      .join ''        # Join the array elements back into a single string

module.exports = Microblog  # Export the Microblog class as a module
