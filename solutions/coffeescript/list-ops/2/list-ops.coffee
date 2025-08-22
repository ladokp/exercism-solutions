class ListOps
  @append: (list1, list2) -> @concat([list1, list2])

  @concat: (lists) ->
    results = []
    for list in lists
      for element in list
        results.push element
    results

  @filter: (list, f) -> (element for element in list when f(element))

  @mylength: (list) -> @foldl(list, ((accumulator, _) -> accumulator + 1), 0)

  @map: (list, f) -> (f(element) for element in list)
  
  @foldl: (list, f, initial) ->
    results = initial
    for element in list
      results = f(results, element)
    results

  @foldr: (list, f, initial) ->
    if list.length == 0
      initial
    else
      last = list.pop()
      @foldr(list, f, f(initial, last))

  @reverse: (list) ->
    if list.length == 0
      list
    else
      (list[index] for index in [(list.length - 1)..0])

module.exports = ListOps
