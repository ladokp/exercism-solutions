# Matrix class to represent a matrix and provide row and column access methods.
class Matrix
  # Constructor to initialize the matrix based on a string description.
  # Each line in the description represents a row, and each number is separated by a space.
  constructor: (description) ->
    # Split the description into rows and then split each row into numbers.
    # Convert each number from string to a number type.
    @rows = description.split("\n").map (row) ->
      row.trim().split(/\s+/).map (number) -> +number
  # Method to get a specific row (1-indexed).
  # Returns the row as an array.
  row: (index) ->
    @rows[index - 1]  # Adjust for 1-indexing
  # Method to get a specific column (1-indexed).
  # Returns the column as an array by extracting the corresponding element from each row.
  column: (index) ->
    @rows.map (row) -> row[index - 1]  # Adjust for 1-indexing
# Export the Matrix class as a module to be used in other files.
module.exports = Matrix
