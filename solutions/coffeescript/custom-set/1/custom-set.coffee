# Class representing a custom set
class CustomSet
  # Constructor: initializes the set with unique values
  # @param {Array} values - Array of initial values, duplicates will be removed
  constructor: (values = []) ->
    # Ensure values are comparable (numbers or strings)
    if not (values.every (value) -> typeof value is 'number' or typeof value is 'string')
      throw new Error("CustomSet only supports numbers or strings")
    
    # Filter out duplicates more efficiently using `reduce`
    @values = values.reduce ((unique, value) ->
      if value not in unique then unique.push value
      unique
    ), []
    
    # Ensure the values are always sorted
    @values.sort()

  # Check if the set is empty
  empty: -> 
    @values.length == 0

  # Check if the set contains a value
  contains: (value) -> 
    @values.indexOf(value) != -1

  # Add a value to the set, maintaining sorted order
  add: (value) ->
    # Ensure the value is comparable (number or string)
    if typeof value isnt 'number' and typeof value isnt 'string'
      throw new Error("CustomSet only supports numbers or strings")
    
    # Return early if the value already exists
    return if @contains(value)
    
    # Insert value in the correct position to maintain sorted order
    i = @binarySearchInsertIndex(value)
    @values.splice(i, 0, value)  # Insert value at the correct position

  # Binary search to find the correct insertion index
  binarySearchInsertIndex: (value) ->
    low = 0
    high = @values.length
    while low < high
      mid = Math.floor((low + high) / 2)
      if @values[mid] < value then low = mid + 1
      else high = mid
    low  # Return the correct index for insertion

  # Check if this set is a subset of another set
  subset: (other) ->
    # Ensure the other object is a valid CustomSet
    if not other instanceof CustomSet
      throw new Error("subset operation requires another CustomSet instance")
    
    @values.every (item) -> other.contains item

  # Check if this set is disjoint with another set (no common elements)
  disjoint: (other) -> 
    # Ensure the other object is a valid CustomSet
    if not other instanceof CustomSet
      throw new Error("disjoint operation requires another CustomSet instance")
    
    @values.every (item) -> !other.contains item

  # Check if this set is equal to another set
  equal: (other) ->
    # Ensure the other object is a valid CustomSet
    if not other instanceof CustomSet
      throw new Error("equal operation requires another CustomSet instance")
    
    return false if @values.length != other.values.length
    # Compare each element since both sets are sorted
    @values.every (item, index) -> item == other.values[index]

  # Find the intersection of this set and another set
  intersection: (other) ->
    # Ensure the other object is a valid CustomSet
    if not other instanceof CustomSet
      throw new Error("intersection operation requires another CustomSet instance")
    
    new CustomSet @values.filter (item) -> other.contains item

  # Find the difference between this set and another set
  difference: (other) -> 
    # Ensure the other object is a valid CustomSet
    if not other instanceof CustomSet
      throw new Error("difference operation requires another CustomSet instance")
    
    new CustomSet @values.filter (item) -> !other.contains item
  
  # Find the union of this set and another set, ensuring no duplicates
  union: (other) ->
    # Ensure the other object is a valid CustomSet
    if not other instanceof CustomSet
      throw new Error("union operation requires another CustomSet instance")
    
    # Concatenate values from both sets, then remove duplicates with reduce
    new CustomSet @values.concat(other.values).reduce ((unique, item) ->
      if item not in unique then unique.push item
      unique
    ), []

# Export the CustomSet class for external use
module.exports = CustomSet
