[
"black",
"brown",
"red",
"orange",
"yellow",
"green",
"blue",
"violet",
"grey",
"white"
] as $colors |
if (.property == "colorCode") then
  .input.color as $color | $colors | index($color)
else
  $colors
end
