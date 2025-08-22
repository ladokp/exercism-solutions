complement = (nuc) ->
    switch nuc
        when "C" then "G"
        when "G" then "C"
        when "T" then "A"
        when "A" then "U"

class RnaTrascription
  constructor: (@dna) ->
  
  toRna: -> (complement(nuc) for nuc in @dna).join("")
        
module.exports = RnaTrascription
