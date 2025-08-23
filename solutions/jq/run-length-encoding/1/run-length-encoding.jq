def encode:
  [scan("(.)(\\1*)")]
  | map(.[1] |= if . == "" then "" else (length + 1 | tostring) end)
  | map(.[1] + .[0])
  | join("")
;

def decode:
  [scan("(\\d*)(\\D)")]
  | map(.[0] |= if . == "" then 1 else (. | tonumber) end)
  | map(.[0] * .[1])
  | join("")
;
