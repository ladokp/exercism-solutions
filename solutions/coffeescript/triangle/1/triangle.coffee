class Triangle
  @isValid: (sides...) ->
    return false unless sides.every((s) -> s > 0)
    maxdex = 0
    for side,i in sides
      maxdex = i if side > sides[maxdex]
    c = sides[maxdex]
    sides.splice(maxdex, 1)
    [a, b] = sides
    
    a + b > c
    
  constructor: (sides...) ->
    @valid = Triangle.isValid(sides...)
    @a = sides[0]
    @b = sides[1]
    @c = sides[2]

  equilateral: ->
    @valid and @a == @b and @b == @c
  
  isosceles: ->
    @valid and (@a == @b or @b == @c or @c == @a)

  scalene: ->
    @valid and @a != @b and @b != @c and @c != @a

module.exports = Triangle
