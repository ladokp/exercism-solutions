class Node
  constructor: (value, next = null) ->
    @value = value
    @next = next

class LinkedList
  constructor: ->
    @head = null
    
  count: 0

  pushNode: (args) ->
    node = new Node args
    @count++
    if !@head?
      @head = node
    else
      last = @head
      while last?.next?
        last = last.next
      last.next = node

  popNode: () ->
    if !@head? then return undefined

    @count--
    
    if !@head.next?
      value = @head.value
      @head = null
      value
    else
      prev = @head
      last = prev.next
      while last.next?
        prev = prev.next
        last = prev.next
      value = last.value
      last = null
      prev.next = null
      value
    

  deleteNode: (args) ->
    if !@head? then return

    prev = @head

    if prev.value == args
      @head = prev.next
      prev = null
      @count--
      return
    
    current = prev.next
    while current?
      if current.value == args
        prev.next = current.next
        @count--
        return
      prev = current
      current = current.next
        

  shiftNode: () ->
    if @head? then @count--
    value = @head?.value
    @head = @head?.next
    value

  unshiftNode: (args) ->
    node = new Node args
    @count++
    if !@head?
      @head = node
    else
      node.next = @head
      @head = node

  countNodes: ->
    if !@head? then 0
    else
      current = @head
      c = 0
      while current?
        c++
        current = current.next
      c

module.exports = LinkedList
