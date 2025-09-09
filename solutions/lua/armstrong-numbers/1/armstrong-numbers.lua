local ArmstrongNumbers = {}

function ArmstrongNumbers.is_armstrong_number(number)
  local sum, digits = 0, tostring(number)
  for index = 1, #digits do
    local digit = digits:sub(index, index)
    sum = sum + tonumber(digit) ^ #digits
  end
  return number == sum
end

return ArmstrongNumbers