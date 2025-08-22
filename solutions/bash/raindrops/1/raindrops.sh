#!/usr/bin/env bash

if [ "$#" -ne 1 ]; then
    echo "Usage: raindrops.sh <number>"
    exit 1
fi

number=$1
result=""
if [ $(expr $number % 3) != "0" -a $(expr $number % 5) != "0" -a $(expr $number % 7) != "0" ]; then
    echo $number
fi
if [ $(expr $number % 3) == "0" ]; then
    result+="Pling"
fi
if [ $(expr $number % 5) == "0" ]; then
    result+="Plang"
fi
if [ $(expr $number % 7) == "0" ]; then
    result+="Plong"
fi
echo $result