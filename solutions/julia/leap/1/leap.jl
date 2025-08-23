"""
    is_leap_year(year)

Return `true` if `year` is a leap year in the gregorian calendar.

"""
function is_leap_year(year)
    modulo_by(n) = year % n == 0
    modulo_by(4) && !modulo_by(100) || modulo_by(400)
end

