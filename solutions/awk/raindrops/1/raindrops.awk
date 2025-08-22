# These variables are initialized on the command line (using '-v'):
# - num

BEGIN {
  if (num % 3 == 0) value = value "Pling"
  if (num % 5 == 0) value = value "Plang"
  if (num % 7 == 0) value = value "Plong"
  print value ? value : num
}
