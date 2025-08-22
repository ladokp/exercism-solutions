#!/usr/bin/env bash

number="$1"

unary () { for ((i=0;i<$1;i++)); do echo -n "I"; done }

number=$(unary "$number")
number=${number//IIIII/V}
number=${number//IIII/IV}
number=${number//VV/X}
number=${number//VIV/IX}
number=${number//XXXXX/L}
number=${number//XXXX/XL}
number=${number//LL/C}
number=${number//LXL/XC}
number=${number//CCCCC/D}
number=${number//CCCC/CD}
number=${number//DD/M}
number=${number//DCD/CM}

echo "$number"
