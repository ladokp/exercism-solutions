# Class representing a bucket with operations to fill, empty, and pour water between buckets
class Bucket
  constructor: (@name, @size) ->
    # Initialize the bucket with a given name and size, starting with no water
    @amount = 0

  # Check if the bucket is full
  isFull: -> @amount is @size

  # Check if the bucket is empty
  isEmpty: -> @amount is 0

  # Determine how much more water the bucket can hold
  available: -> @size - @amount

  # Fill the bucket to its maximum capacity
  fill: -> @amount = @size

  # Empty the bucket completely
  empty: -> @amount = 0

  # Pour water from this bucket into another bucket
  pourInto: (other) ->
    quantity = Math.min @amount, other.available()
    @amount -= quantity
    other.amount += quantity

# Function to compute the greatest common divisor (GCD) of two numbers
gcd = (a, b) -> if b == 0 then a else gcd(b, a % b)

# Class to solve the TwoBucket problem, where two buckets of different sizes are used
# to measure a specific amount of water with the minimum number of moves
class TwoBucket
  constructor: ({bucketOne, bucketTwo, @goal, startBucket}) ->
    # Validate if the problem is solvable based on bucket sizes and the goal
    throw new Error 'impossible' if @goal > Math.max(bucketOne, bucketTwo)

    # If the goal is not a multiple of the GCD of the bucket sizes, it's impossible
    g = gcd(bucketOne, bucketTwo)
    throw new Error 'impossible' if @goal % g isnt 0

    # Initialize the two buckets
    @first = new Bucket("one", bucketOne)
    @second = new Bucket("two", bucketTwo)

    # Swap if starting with bucket two
    if startBucket == "two"
      [@first, @second] = [@second, @first]

  # Measure method to calculate the number of moves required to reach the goal
  measure: ->
    moves = 0  # Track the number of moves

    # Start by filling the first bucket
    @first.fill()
    moves++

    # Check if the second bucket can directly achieve the goal
    if @second.size == @goal
      @second.fill()
      moves++

    # Begin the process of transferring water between the buckets
    loop
      # Check if either bucket has reached the goal
      return @result(@first, @second, moves) if @first.amount == @goal
      return @result(@second, @first, moves) if @second.amount == @goal

      # If the first bucket is empty, fill it
      if @first.isEmpty()
        @first.fill()
      # If the second bucket is full, empty it
      else if @second.isFull()
        @second.empty()
      # Otherwise, pour from the first bucket into the second
      else
        @first.pourInto @second

      moves++  # Increment the move counter after each action

  # Return the result once the goal is reached, including the number of moves,
  # the bucket that holds the goal amount, and the amount of water in the other bucket
  result: (winner, loser, moves) ->
    { moves: moves, goalBucket: winner.name, otherBucket: loser.amount }

# Export the TwoBucket class for use in other modules or test cases
module.exports = TwoBucket
