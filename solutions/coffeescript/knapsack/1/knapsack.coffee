# Algorithm taken from Wikipedia: https://en.wikipedia.org/wiki/Knapsack_problem

class Knapsack
  @maximumValue: ({maximumWeight, items}) ->
    return 0 if items.length is 0
    
    # Initialize dynamic programming table
    dpTable = Array.from({length: items.length + 1}, -> new Array(maximumWeight + 1))
    dpTable[0].fill 0

    for itemIndex in [1 .. items.length]
      {weight, value} = items[itemIndex - 1]
      for remainingWeight in [0 .. maximumWeight]
        if weight > remainingWeight
          dpTable[itemIndex][remainingWeight] = dpTable[itemIndex - 1][remainingWeight]
        else
          dpTable[itemIndex][remainingWeight] = Math.max(
            dpTable[itemIndex - 1][remainingWeight],
            dpTable[itemIndex - 1][remainingWeight - weight] + value
          )

    dpTable[items.length][maximumWeight]

module.exports = Knapsack
