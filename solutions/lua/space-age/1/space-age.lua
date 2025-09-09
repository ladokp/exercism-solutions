local space_age = {}
    
function space_age:new(seconds)
  local function calculator(earth_year_factor)
    earth_year_seconds = 31557600
    return function() return tonumber(string.format("%.2f", seconds / earth_year_seconds / earth_year_factor)) end
  end
    
  return {
    seconds = seconds,
    on_earth = calculator(1),
    on_mercury = calculator(0.2408467),
    on_venus = calculator(0.61519726),
    on_mars =  calculator(1.8808158),
    on_jupiter = calculator(11.862615),
    on_saturn = calculator(29.447498),
    on_uranus = calculator(84.016846),
    on_neptune = calculator(164.79132)
  }
end

return space_age