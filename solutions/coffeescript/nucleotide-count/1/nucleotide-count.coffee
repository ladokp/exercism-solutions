class NucleotideCount
  constructor: (dna) ->
    @nucleotideCounts = {A: 0, T: 0, C: 0, G: 0}
    for nuc in dna
      if nuc not of @nucleotideCounts
        throw "Invalid nucleotide strand"
      @nucleotideCounts[nuc] += 1

  count: (nuc) -> 
    if nuc not of @nucleotideCounts
      throw "Invalid nucleotide"
    @nucleotideCounts[nuc]

module.exports = NucleotideCount
