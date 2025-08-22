#!/usr/bin/env bash

if [ "$#" -ne 1 ]; then
    echo "Usage: raindrops.sh <number>"
    exit 1
fi

number=$1
result=""
if [ $((number % 3)) != "0" ] && [ $((number % 5)) != "0" ] && [ $((number % 7)) != "0" ]; then
    echo "$number"
fi
if [ $((number % 3)) == "0" ]; then
    result+="Pling"
fi
if [ $((number % 5)) == "0" ]; then
    result+="Plang"
fi
if [ $((number % 7)) == "0" ]; then
    result+="Plong"
fi
echo "$result"