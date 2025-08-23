def count_bits(count):
  if . == 0 then
    count
  else
    [(./2 | floor), .%2] as [$next, $bit]
    | $next | count_bits(count+$bit)
  end
;

.number | count_bits(0)
