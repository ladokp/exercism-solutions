class PascalsTriangle
  rows: (rows) ->
    if (rows <= 0) then [] else @getTriangle rows

  getTriangle: (rows) ->
    last = @getRow(last) for index in [1..rows]

  getRow: (last = []) ->
    @getNumber(last[i-1], last[i]) for i in [0..last.length]

  getNumber: (x, y) -> x + y || 1


module.exports = PascalsTriangle