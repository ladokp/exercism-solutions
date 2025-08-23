def validate:
  def gcd: if .[1] == 0 then .[0] else [.[1], .[0] % .[1]] | gcd end;

  ([.bucketOne, .bucketTwo] | max) as $max |
  ([.bucketOne, .bucketTwo] | gcd) as $gcd |
  if (.goal > $max) or ($gcd != 1 and .goal % $gcd != 0)
    then ("impossible" | halt_error)
    else .
  end;

def solve($goal):
  def _solve:
    if .[0].amount == $goal then {moves: .[2], goalBucket: .[0].name, otherBucket: .[1].amount}
    elif .[1].amount == $goal then {moves: .[2], goalBucket: .[1].name, otherBucket: .[0].amount}
    else
      if .[0].amount == 0 then .[0].amount = .[0].size
      elif .[1].size == $goal then .[1].amount = .[1].size
      elif .[1].amount == .[1].size then .[1].amount = 0
      else
        ([.[0].amount, .[1].size - .[1].amount] | min) as $quantity |
        .[0].amount -= $quantity |
        .[1].amount += $quantity
      end
      | .[2] += 1 | _solve
    end;
  . + [0] | _solve;

validate |
.goal as $goal |
{ name: "one", amount: 0, size: .bucketOne } as $one |
{ name: "two", amount: 0, size: .bucketTwo } as $two |
if .startBucket == "one"
  then [$one, $two]
  else [$two, $one]
end | solve($goal)