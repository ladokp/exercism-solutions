class CircularBuffer
  constructor: (@capacity) -> @data = []

  write: (value) ->
    throw new Error "full buffer" if @data.length == @capacity
    @data.push value

  read: () ->
    throw new Error "empty buffer" if !@data.length
    [head, @data...] = @data
    head

  overwrite: (value) ->
    @read() if @data.length == @capacity
    @write value

  clear: () -> @data = []

module.exports = CircularBuffer