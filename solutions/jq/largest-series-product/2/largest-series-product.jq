def assert(cond; msg):
  if cond then . else (msg | halt_error(1)) end
;

def product:
  reduce .[] as $n (1; . * $n)
;

# Ensure the assertions and process the input
. 
| assert(.span >= 0; "span must not be negative")
| assert(.span <= (.digits | length); "span must not exceed string length")
| assert(.digits | test("^[0-9]*$"); "digits input must only contain digits")
| .span as $span
| (.digits | explode | map(. - 48)) as $digits  # Convert characters to their numeric values
| [
    range((($digits | length) - $span) + 1)
    | $digits[. : . + $span]
    | product
  ]
| max
