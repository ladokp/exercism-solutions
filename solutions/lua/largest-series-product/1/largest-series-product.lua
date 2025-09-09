-- Convert a string of digits to an array of numbers
local function digit_string_to_array(digit_string)
  local a = {}
  digit_string:gsub('.', function(c)
    assert(tonumber(c), "Series must contain only digits")
    table.insert(a, tonumber(c))
  end)
  return a
end

-- Calculate the product of a subarray from 'first' to 'last' indices
local function series_product(a, first, last)
  local product = 1
  for i = first, last do
    product = product * a[i]
  end
  return product
end

-- Main function to find the largest product in a series
return function(config)
  assert(config.span >= 0, "Span must be non-negative")
  assert(config.span <= #config.digits, "Span must not be greater than the length of the digits string")

  local digits = digit_string_to_array(config.digits)
  local largest

  -- Iterate over each possible substring of length 'span' and calculate the product
  for i = 1, #digits - config.span + 1 do
    local product = series_product(digits, i, i + config.span - 1)
    largest = math.max(largest or 0, product)
  end

  -- Handle the case where span is 0: the largest product is defined as 1 (empty product)
  return largest or 1
end
