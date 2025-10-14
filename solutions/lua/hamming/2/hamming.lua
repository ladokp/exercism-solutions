local Hamming = {}

function Hamming.compute(a, b)
    assert(#a == #b, 'strands must be of equal length')
    distance = 0
    for index = 1, #a do
        if a:sub(index, index) ~= b:sub(index, index) then distance = distance + 1 end
    end
    return distance
end

return Hamming