local PopCount = {}
    
function PopCount.egg_count(number)
    local eggs = 0
    while number > 0 do
        eggs = eggs + (number % 2)
        number = (number - (number % 2)) / 2
    end
    return eggs
end

return PopCount