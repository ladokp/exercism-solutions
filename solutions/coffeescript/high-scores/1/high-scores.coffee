class HighScores
  constructor: (@scores) ->

  latest: () ->
    @scores[..].pop()

  personalBest: () ->
    Math.max.apply null, @scores

  personalTopThree: () ->
    @scores.slice()
           .sort (x, y) => y-x
           .slice 0, 3

module.exports = HighScores
