return function(array, target)
  local high, low = #array, 1
  while low <= high do
    local mid = math.floor((low + high) / 2)
    if array[mid] == target then
      return mid
    elseif array[mid] < target then
      low = mid + 1
    else
      high = mid - 1
    end
  end
  return -1
end