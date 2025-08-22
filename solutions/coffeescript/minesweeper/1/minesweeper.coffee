class Minesweeper
  @annotate: (minefield) ->
    return minefield if minefield.length < 1 or minefield[0].length < 1
    
    board = minefield.map (row) -> row.split ''

    annotatedBoard = board.map (row, x) ->
      row.map (cell, y) ->
        if cell == '*'
          cell
        else
          count = 0
          for i in [-1..1]
            for j in [-1..1]
              nx = x + i
              ny = y + j
              if nx >= 0 and nx < board.length and ny >= 0 and ny < row.length
                count += 1 if board[nx][ny] == '*'
          if count == 0 then ' ' else count.toString()
      .join ''

module.exports = Minesweeper
