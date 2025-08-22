textify = (number) ->
    switch number
        when 0 then "no"
        when 1 then "one"
        when 2 then "two"
        when 3 then "three"
        when 4 then "four"
        when 5 then "five"
        when 6 then "six"
        when 7 then "seven"
        when 8 then "eight"
        when 9 then "nine"
        when 10 then "ten"

capitalize = (text) ->
    text.charAt(0).toUpperCase() + text.slice(1);

pluralBottles = (number) ->
    if number == 1
        "bottle"
    else
        "bottles"
    
class BottleSong
  verse: (number) -> 
    num = textify(number)
    line1 = "#{capitalize(num)} green #{pluralBottles(number)} hanging on the wall,"
        
    """
    #{line1}
    #{line1}
    And if one green bottle should accidentally fall,
    There'll be #{textify(number-1)} green #{pluralBottles(number-1)} hanging on the wall.
    """
    
    
  recite: (start, verses) ->
     (@verse(number) for number in [start...start-verses]).join("\n\n").split("\n").flat()

module.exports = BottleSong