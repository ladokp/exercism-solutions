def colors: ["black", "brown", "red", "orange", "yellow", "green", "blue", "violet", "grey", "white"];
def units: ["gigaohms", "megaohms", "kiloohms", "ohms"];

def resistor_color: . as $c | colors | index($c);

def normalize($units):
  pow(10; 3 * (($units | length) - 1)) as $divider
  | if .value >= $divider or $divider == 1 then {
    "value": (.value / $divider),
    "unit": $units[0]
  } else normalize($units[1:]) end
;

{
  "value": (
    (.colors[:2] | reduce .[] as $color (0; . * 10 + ($color | resistor_color)))
    * pow(10; .colors[2] | resistor_color)
  )
}
| normalize(units)