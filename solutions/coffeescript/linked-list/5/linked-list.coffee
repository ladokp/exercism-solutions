class Node
  constructor: (value, next = null) ->
    @value = value
    @next = next

class LinkedList
  constructor: (head = null) ->
    @head = head
    @count = 0

  pushNode: (value) ->
    node = new Node value
    @count++ 
    if !@head?
      @head = node
    else
      last = @head
      while last?.next?
        last = last.next
      last.next = node

  popNode: ->
    if !@head? then return undefined

    @count--
    
    if !@head.next?
      value = @head.value
      @head = null
      value
    else
      previous = @head
      last = previous.next
      while last.next?
        previous = previous.next
        last = previous.next
      value = last.value
      last = null
      previous.next = null
      value
    

  deleteNode: (value) ->
    if !@head? then return

    previous = @head

    if previous.value == value
      @head = previous.next
      previous = null
      @count--
      return
    
    current = previous.next
    while current?
      if current.value == value
        previous.next = current.next
        @count--
        return
      previous = current
      current = current.next
        

  shiftNode: ->
    if @head? then @count--
    value = @head?.value
    @head = @head?.next
    value

  unshiftNode: (value) ->
    node = new Node value
    @count++ 
    if !@head?
      @head = node
    else
      node.next = @head
      @head = node

  countNodes: ->
    @count

module.exports = LinkedList