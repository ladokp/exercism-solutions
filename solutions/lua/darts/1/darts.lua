local Darts = {}

function Darts.score(x, y)
    local distance = (x*x+y*y)^0.5
    if distance <= 1 then return 10 end
    if distance <= 5 then return 5 end
    if distance <= 10 then return 1 end
    return 0
end

return Darts
