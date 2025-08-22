module.exports = class Hexadecimal
  constructor: (@hex) ->

  toDecimal: ->
    return 0 unless @hex.match /[0-9a-fA-F]$/
    parseInt @hex, 16