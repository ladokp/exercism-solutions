# Class to flatten nested arrays into a single-level array
class FlattenArray
  # Method to recursively flatten an array, ignoring `null` and `undefined` values.
  # @param {Array} values - The array that may contain nested arrays, `null`, or `undefined` values.
  # @returns {Array} A single-level array with all nested arrays flattened and `null`/`undefined` values removed.
  @flatten: (values) ->
    values
      # Filter out null or undefined values
      .filter (value) -> value?
      # Flatten the array
      .reduce (flattened, value) ->
        if Array.isArray value
          # Recursively flatten nested arrays
          flattened.concat FlattenArray.flatten value
        else
          # Add the non-array value to the flattened array
          flattened.concat value
      , []

# Export the FlattenArray class for external use
module.exports = FlattenArray
