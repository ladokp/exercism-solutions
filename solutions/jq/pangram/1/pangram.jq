def is_pangram:
  [range(97; 123)] as $alphabet
  | ascii_downcase
  | explode
  | map(select(IN($alphabet[])))
  | unique == $alphabet;

.sentence | is_pangram
