local function sum_of_multiples(numbers)
  return {
    to = function(number)
        local sum = 0
        for index1=1,number-1 do
          for _, value in ipairs(numbers) do
            if index1 % value == 0 then
              sum = sum + index1
              break
            end
          end
        end
        return sum
      end
  }
end

return sum_of_multiples