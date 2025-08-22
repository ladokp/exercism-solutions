class Hamming
  @distance: (strand1, strand2) ->
    if strand1.length != strand2.length
      throw new Error "strands must be of equal length"
    
    strand2.split("")
           .filter (element, index) ->
              element != strand1[index]
           .length

module.exports = Hamming
