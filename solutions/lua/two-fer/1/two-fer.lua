local TwoFer = {}

function TwoFer.two_fer(name)
    if name == nil then name = "you" end
    return string.format("One for %s, one for me.", name)
end

return TwoFer
