function merge(table1, table2) for key, value in pairs(table2) do table.insert(table1, value) end return table1 end

local function flatten (array)
  local return_value = {}
  for _, value in ipairs(array) do
    if type(value) == 'table' then
      merge(return_value, flatten(value))
    else
      table.insert(return_value, value)
    end
  end
  return return_value
end

return flatten