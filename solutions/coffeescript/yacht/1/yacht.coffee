class Yacht
  @score: (dice, category) ->
    scoreMap =
      yacht: -> if unique(dice).length == 1 then 50 else 0
      ones: -> count(dice, 1) * 1
      twos: -> count(dice, 2) * 2
      threes: -> count(dice, 3) * 3
      fours: -> count(dice, 4) * 4
      fives: -> count(dice, 5) * 5
      sixes: -> count(dice, 6) * 6
      'full house': ->
        counts = dice.reduce((acc, val) ->
          acc[val] = (acc[val] or 0) + 1
          acc
        , {})
        if Object.values(counts).sort().toString() == '2,3'
          return dice.reduce (a, b) ->
              a + b
            , 0
        0
      'four of a kind': ->
        for n in [1..6]
          return 4 * n if count(dice, n) >= 4
        0
      'little straight': ->
        sorted = unique(dice).sort()
        if isEqual(sorted, [1, 2, 3, 4, 5]) then 30 else 0
      'big straight': ->
        sorted = unique(dice).sort()
        if isEqual(sorted, [2, 3, 4, 5, 6]) then 30 else 0
      choice: -> dice.reduce (a, b) ->
        a + b
      , 0

    (scoreMap[category] or -> 0)()

unique = (arr) ->
  result = []
  for element in arr
    result.push(element) unless element in result
  result

count = (arr, element) -> arr.filter((x) -> x == element).length

isEqual = (arr1, arr2) -> arr1.sort().toString() == arr2.sort().toString()

module.exports = Yacht
