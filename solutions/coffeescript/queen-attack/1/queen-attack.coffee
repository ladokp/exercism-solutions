class QueenAttack
  constructor: (args) ->
    @white = args and args.white or [0,3]
    @black = args and args.black or [7,3]
    throw 'Queens cannot share the same space' if @white[0] == @black[0] and @white[1] == @black[1]
  toString: ->
    board = ('_' for _ in [0...8] for _ in [0...8])
    board[@white[1]][@white[0]] = 'W'
    board[@black[1]][@black[0]] = 'B'
    (row.join(' ') for row in board).join('\n')
  canAttack: ->
    x = Math.abs(@black[1] - @white[1])
    y = Math.abs(@black[0] - @white[0])
    x == y or x == 0 or y == 0
module.exports = QueenAttack