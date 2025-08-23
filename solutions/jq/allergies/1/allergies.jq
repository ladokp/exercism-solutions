# Function to calculate 2^pos
def exp2($pos):
  if $pos == 0 then 1
  else 2 * exp2($pos - 1)
  end
;

# Shift right function, divides n by 2^pos and floors the result
def shift_right($n; $pos):
  ($n / exp2($pos)) | floor
;

# Function to get the bit at a specific position
def bit_at($n; $pos):
  shift_right($n; $pos) % 2
;

# Generates a list of allergies based on the score
def allergiesList:
  .score as $score
  | [ "eggs", "peanuts", "shellfish", "strawberries",
      "tomatoes", "chocolate", "pollen", "cats" ]
  | to_entries
  | map(select(bit_at($score; .key) == 1) | .value)
;

# Checks if the item is in the list of allergies based on the score
def allergicTo:
  .item as $item
  | allergiesList as $as
  | $item | IN($as[])
;

# Main function to determine output based on property
def main:
  if .property == "list" then (.input | allergiesList)
  elif .property == "allergicTo" then (.input | allergicTo)
  else empty
  end
;

# Entry point
main
