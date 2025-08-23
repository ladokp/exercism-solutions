def save_double:
  if . == null
  then 0 
  else 
    . * 2 | 
    if . >= 10
    then . - 9
    else .
    end
  end
;

explode |
map(select(. != 32)) |
map(. - 48) |
(length > 1) and 
  all(. < 10) and
  ([reverse | _nwise(2) | .[0] + (.[1] | save_double)] | add | . % 10 == 0) 