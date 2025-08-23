def tree:
  if .preorder | length == 0 then {}
  else (.preorder | first) as $root | (.inorder | index($root)) as $index |
    {v: $root, l: {preorder: .preorder[1:$index+1], inorder: .inorder[:$index]} | tree, r: {preorder: .preorder[$index+1:], inorder: .inorder[$index+1:]} | tree}
  end;

if   (.preorder | length) != (.inorder | length) then "traversals must have the same length" | halt_error
elif (.preorder | sort) != (.inorder | sort) then "traversals must have the same elements" | halt_error
elif (.preorder | (sort != unique)) then "traversals must contain unique items" | halt_error
else tree end