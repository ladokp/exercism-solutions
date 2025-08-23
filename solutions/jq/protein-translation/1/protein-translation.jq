def lookuptable:
{
    "AUG": "Methionine",
    "UUU, UUC": "Phenylalanine",
    "UUA, UUG": "Leucine",
    "UCU, UCC, UCA, UCG": "Serine",
    "UAU, UAC": "Tyrosine",
    "UGU, UGC": "Cysteine",
    "UGG": "Tryptophan",
    "UAA, UAG, UGA": "STOP"
} | with_entries({
    "key": (.key / ", ")[],
    "value": .value
});

.strand
| lookuptable as $table
| [scan(".{1,3}")]
| map($table[.])
| .[:index("STOP")]
| if index(null)
  then "Invalid codon" | halt_error
  else .
  end