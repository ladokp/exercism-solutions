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
] as $colors
| .colors[:2]
| map(. as $color | $colors | index($color))
| join("")
| tonumber
