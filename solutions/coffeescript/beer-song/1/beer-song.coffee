class BeerSong
  verse: (number) ->
        if number > 2
            result = "#{number} bottles of beer on the wall, #{number} bottles of beer. Take one down and pass it around, #{number-1} bottles of beer on the wall."  
        else if number == 2
            result = "#{number} bottles of beer on the wall, #{number} bottles of beer. Take one down and pass it around, #{number-1} bottle of beer on the wall."      
        else if number == 1
            result = "#{number} bottle of beer on the wall, #{number} bottle of beer. Take it down and pass it around, no more bottles of beer on the wall."        
        else if number == 0
            result = "No more bottles of beer on the wall, no more bottles of beer. Go to the store and buy some more, 99 bottles of beer on the wall."                

  sing: (start, end=0) ->
        result = ""
        for number in [start...end - 1] by -1
            result = result + " " + @verse(number)
        result[1..-1]

module.exports = BeerSong
