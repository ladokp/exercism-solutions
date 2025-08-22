module.exports = class WordCount
  constructor: (@text) ->
  count: ->
    @text.toLowerCase()
         .match(/\w+('\w+)?/g)
         .reduce (entries, word) ->
           entries[word] = entries[word] + 1 or 1
           entries
         , {}