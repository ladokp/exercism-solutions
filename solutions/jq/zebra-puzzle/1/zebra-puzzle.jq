def nextdoor(h1; h2): (h1.number - h2.number) as $d | $d == 1 or $d == -1;

def distinct:
    (unique_by(.man) | length) == 5 and
    (unique_by(.pet) | length) == 5 and
    (unique_by(.smokes) | length) == 5 and
    (unique_by(.drink) | length) == 5 and
    (unique_by(.color) | length) == 5;

def good_street:
    [{h1: .[], h2: .[]}] |
    all ((.h1.color != "ivory" or
          .h2.number != .h1.number + 1 or .h2.color == "green") and
         (.h1.smokes != "Chesterfield" or .h2.pet != "fox" or nextdoor(.h1; .h2)) and
         (.h1.smokes != "Kool" or .h2.pet != "horse" or nextdoor(.h1; .h2)));

.property as $property |
["Japanese", "Norwegian", "English", "Spanish", "Ukrainian"] as $occupant |
["dog", "snails", "fox", "horse", "zebra"] as $pet |
["Chesterfield", "Old Gold", "Parliament", "Lucky Strike", "Kool"] as $smokes |
["milk", "tea", "water", "orange juice", "coffee"] as $drink |
["ivory", "yellow", "green", "blue", "red"] as $color |
[{ number: range(1; 6), man: $occupant[], pet: $pet[],
   smokes: $smokes[], drink: $drink[], color: $color[] }] |
map(select((.man == "English") == (.color == "red") and
           (.man == "Spanish") == (.pet == "dog") and
           (.drink == "coffee") == (.color == "green") and
           (.man == "Ukrainian") == (.drink == "tea") and
           (.smokes == "Old Gold") == (.pet == "snails") and
           (.smokes == "Kool") == (.color == "yellow") and
           (.drink == "milk") == (.number == 3) and
           (.man == "Norwegian") == (.number == 1) and
           (.smokes == "Lucky Strike") == (.drink == "orange juice") and
           (.man == "Japanese") == (.smokes == "Parliament") and
           (.color == "blue") == (.number == 2) and
           ((.smokes != "Chesterfield") or (.pet != "fox")) and
           ((.smokes != "Kool") or (.pet != "horse")) and
           ((.color != "ivory") or (.number != 1 and .number != 5)))) |
[map(select(.number == 1)), map(select(.number == 2)),
 map(select(.number == 3)), map(select(.number == 4)),
 map(select(.number == 5))] |
combinations | select(distinct) | select(good_street) |
if $property == "drinksWater" then
    map(select(.drink == "water"))
else
    map(select(.pet == "zebra"))
end | first | .man