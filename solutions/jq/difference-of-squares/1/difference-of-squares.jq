def seq: [range(1; . + 1)];
def square: . * .;
def squareOfSum: seq | add | square ;
def sumOfSquares: seq | map(square) | add ;

.property as $f
| .input.number
| if   $f == "squareOfSum" then squareOfSum
  elif $f == "sumOfSquares" then sumOfSquares
  else (squareOfSum - sumOfSquares)
  end
