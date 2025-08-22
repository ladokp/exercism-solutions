class Acronym
  @abbreviate: (phrase) ->
    phrase
      .toUpperCase()
      .replace "'", ""
      .match /^[A-Z]|(?<=[^A-Z])[A-Z]/g
      .join ""

module.exports = Acronym