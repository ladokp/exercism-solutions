class Darts
  score: (x, y) ->
    distance = Math.sqrt(Math.pow(x, 2) + Math.pow(y, 2))
    return 10 if distance <= 1
    return 5 if distance <= 5
    return 1 if distance <= 10
    return 0
    

module.exports = Darts
