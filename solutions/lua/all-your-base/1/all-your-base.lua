local all_your_base = {}

all_your_base.convert = function(from_digits, from_base)
    assert(from_base > 1, 'invalid input base')
    local value = 0
    for _, digit in ipairs(from_digits) do
        assert(digit < from_base, 'digit out of range')
        assert(digit >= 0, 'negative digits are not allowed')
        value = value * from_base + digit
    end
    
    return {
        to = function(base)
            assert(base > 1, 'invalid output base')
            local ans, n = {}, value
            while n > 0 do
                table.insert(ans, 1, n % base)
                n = n // base
            end
            return next(ans) and ans or {0}
        end
    }
end

return all_your_base