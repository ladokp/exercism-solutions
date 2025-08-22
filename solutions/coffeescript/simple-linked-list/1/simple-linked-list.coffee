class Element
  constructor: (@value, @next = null) ->

class LinkedList
  constructor: (values = []) ->
    @count = 0
    @head = null
    for value in values when values?
      @add new Element value

  length: ->
    @count

  add: (element) ->
    element.next = @head
    @head = element
    @count += 1

  toArray: ->
    elements = []
    current = @head
    while current?
      elements.push current.value
      current = current.next
    elements

  reverse: (prev = null) ->
    if not @head?
      return @
    
    current = @head
    @head = @head.next
    current.next = prev
    
    if @head?
      @reverse current
    else
      @head = current
    
    @

module.exports.Element = Element
module.exports.LinkedList = LinkedList
