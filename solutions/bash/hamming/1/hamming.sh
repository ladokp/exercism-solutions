#!/usr/bin/env bash

if [ "$#" -eq 2 ]; then
    if [ ${#1} -ne ${#2} ]; then
        echo "strands must be of equal length"
        exit 2
    fi
    distance=0
    for index in $(seq 0 $((${#1} - 1))); do
      if [[ "${1:$index:1}" != "${2:$index:1}" ]]; then
        ((distance++))
      fi
    done
    echo "$distance"
    exit
fi

echo "Usage: hamming.sh <string1> <string2>"
exit 1