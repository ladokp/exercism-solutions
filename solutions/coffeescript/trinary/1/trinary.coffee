module.exports = class Trinary
  constructor: (@digits) ->
  reduceFunction: (accumulator, current, index, array) ->
    accumulator + parseInt(current) * Math.pow(3, array.length - 1 - index)
  toDecimal: ->
    (@digits.split("").reduceRight this.reduceFunction, 0) or 0