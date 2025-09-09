local function square_of_sum(number)
    return (number * (number+1) / 2)^2
end

local function sum_of_squares(number)
    return number * (number + 1) * (2 * number + 1) / 6
end

local function difference_of_squares(number)
    return square_of_sum(number) - sum_of_squares(number)
end

return {
  square_of_sum = square_of_sum,
  sum_of_squares = sum_of_squares,
  difference_of_squares = difference_of_squares
}
