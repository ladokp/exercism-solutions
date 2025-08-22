class Isogram
  @isIsogram: (phrase) ->
    seen = {}
    for character in phrase.toLowerCase().split("-")
                     .join("").split(" ").join("")
        if character of seen
            return false
        seen[character] = true
    return true

module.exports = Isogram