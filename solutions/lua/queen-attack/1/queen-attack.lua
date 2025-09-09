return function(position)
  assert(0 <= position.column and position.column <= 7)
  assert(0 <= position.row and position.row <= 7)
  return {
    row = position.row,
    column = position.column,
    can_attack = function(other)
      return position.row == other.row or
        position.column == other.column or
        position.row - position.column == other.row - other.column or
        position.row + position.column == other.row + other.column
    end,
  }
end