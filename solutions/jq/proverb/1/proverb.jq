.strings as $items
| [range(.strings | length - 1)]
| map("For want of a \($items[.]) the \($items[.+1]) was lost.")
| if $items[0]
  then . + ["And all for the want of a \($items[0])."]
  else [] end
