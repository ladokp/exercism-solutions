local Hamming = {}

function Hamming.compute(a, b)
    distance = 0
    if #a ~= #b then return -1 end
    for index = 1, #a do
        if a:sub(index, index) ~= b:sub(index, index) then distance = distance + 1 end
    end
    return distance
end

return Hamming