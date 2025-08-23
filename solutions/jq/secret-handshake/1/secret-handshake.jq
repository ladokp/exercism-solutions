# Define a function to compute power of 2
def pow2($exp):
  reduce range(0; $exp) as $i (1; . * 2)
;

# Define a function to shift the bits of a number to the right
def shift_right($number; $positions):
  $number / pow2($positions) | floor
;

# Define a function to check if a bit is set at a specific position
def bit_set($number; $positions):
  shift_right($number; $positions) % 2 == 1
;

# Main logic
.number as $number
| ["wink", "double blink", "close your eyes", "jump"] as $actions
| $actions
| to_entries
| map(select(bit_set($number; .key)) | .value)
| if bit_set($number; $actions | length) then reverse else . end
