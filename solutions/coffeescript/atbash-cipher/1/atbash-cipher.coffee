swap = (character) ->
    if (97 <= character.charCodeAt(0) < 123) or (65 <= character.charCodeAt(0) < 91)
        String.fromCharCode(122 - (character.charCodeAt(0) - 97))
    else
        character

mirror = (str) -> (swap(character) for character in str.toLowerCase() when (97 <= character.charCodeAt(0) < 123) or (65 <= character.charCodeAt(0) < 91) or (48 <= character.charCodeAt(0) < 58))
        
class AtbashCipher
  @encode: (phrase) ->
    groupSize = 5
    encoding = mirror(phrase)
    slices = Math.ceil(encoding.length / groupSize)
    (encoding.slice(groupSize*i, groupSize*i+groupSize).join("") for i in [0...slices]).join(" ")

  @decode: (phrase) ->
    mirror(phrase).join("")

module.exports = AtbashCipher