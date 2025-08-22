class ProteinTranslation
  mappings =
    'AUG': 'Methionine'
    'UUU': 'Phenylalanine'
    'UUC': 'Phenylalanine'
    'UUA': 'Leucine'
    'UUG': 'Leucine'
    'UCU': 'Serine'
    'UCC': 'Serine'
    'UCA': 'Serine'
    'UCG': 'Serine'
    'UAU': 'Tyrosine'
    'UAC': 'Tyrosine'
    'UGU': 'Cysteine'
    'UGC': 'Cysteine'
    'UGG': 'Tryptophan'
    'UAA': 'STOP'
    'UAG': 'STOP'
    'UGA': 'STOP'

  @proteins: (strand) ->
    codons = []
    while strand.length >= 1
      codons.push strand.slice 0, 3
      strand = strand.slice 3
  
    results = []
    for codon in codons
      throw 'Invalid codon' if not (codon of mappings)
      protein = mappings[codon]
      return results if protein == 'STOP'        
      results.push protein  
    results

module.exports = ProteinTranslation