def mapping:
  {
    "AEIOULNRST": 1,
    "DG": 2,
    "BCMP": 3,
    "FHVWY": 4,
    "K": 5,
    "JX": 8,
    "QZ": 10,
  } | with_entries({"key": (.key | split("") | .[]), value})
;
  
.word
| ascii_upcase / ""
| map(mapping[.])
| add // 0
