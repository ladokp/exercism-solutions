Classification =
  equal: 'equal'
  unequal: 'unequal'
  sublist: 'sublist'
  superlist: 'superlist'

class Sublist  
  @is_superlist: (listOne, listTwo) ->
    for index in [0..listOne.length - listTwo.length]
      if listOne.slice(index, index + listTwo.length).join() == listTwo.join()
        return true
    false

  @classify: (listOne, listTwo) ->
    if JSON.stringify(listOne) == JSON.stringify(listTwo)
      Classification.equal
    else if @is_superlist(listTwo, listOne)
      Classification.sublist
    else if @is_superlist(listOne, listTwo)
      Classification.superlist
    else
      Classification.unequal

module.exports = { Sublist, Classification }