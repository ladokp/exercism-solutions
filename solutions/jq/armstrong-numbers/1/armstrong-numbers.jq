def ipow(num; power):
  # Calculate integer power using reduce
  reduce range(power) as $i (1; . * num)
;

def armstrong_sum:
  # Calculate the width of the number
  (log10 | floor + 1) as $width
  # Reduce to calculate the Armstrong sum
  | reduce range($width) as $i ([0, .]; 
      . as [$sum, $num]
      | [ $sum + ipow($num % 10; $width)
        , ($num / 10 | floor)
      ]
    )
  | first
;

# Compare the number with its Armstrong sum
.number | armstrong_sum == .
