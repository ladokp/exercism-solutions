local function is_prime(number)
  for i = 3, math.sqrt(number), 2 do
    if number % i == 0 then return false end
  end
  return true
end

return function(n)
  assert(n > 0)
  if n == 1 then return 2 else n = n - 1 end
  local test = 3
  while (true) do
    if is_prime(test) then n = n-1 end
    if n == 0 then break else test = test + 2 end
  end
  return test
end