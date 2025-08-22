class Gigasecond
  @add: (moment) ->
    new Date(moment.getTime() + 1e12)

module.exports = Gigasecond
