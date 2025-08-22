class BinarySearch
  constructor: (@values) ->
    @start = 0
    @end = @values.length - 1
    @mid = (@start + @end) // 2

  find: (value) ->
    while @start <= @end
        current = @values[@mid]
        if value == current
            return @mid
        else if value <= current
            @end = @mid - 1
        else if value >= current
            @start = @mid + 1
        @mid = (@start + @end) // 2

    throw new Error 'value not in array'

module.exports = BinarySearch