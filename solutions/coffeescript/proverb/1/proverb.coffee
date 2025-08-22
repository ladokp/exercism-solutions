class Proverb
  @recite: (items) ->  
    proverb = ""
    if items.length
      for index in [0...items.length - 1]
        proverb += "For want of a #{items[index]} the #{items[index + 1]} was lost.\n"
      proverb += "And all for the want of a #{items[0]}."
    proverb

module.exports = Proverb
