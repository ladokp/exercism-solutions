class BinarySearchTree
  constructor: (@data) ->

  insert: (value) ->
    if value <= @data
      @left?.insert(value) || @left = new BinarySearchTree(value)
    else
      @right?.insert(value) || @right = new BinarySearchTree(value)
  
  each: (callback) -> 
    @left.each callback if @left
    callback @data
    @right.each callback if @right

module.exports = BinarySearchTree