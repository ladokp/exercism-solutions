#!/usr/bin/env bash

main () {
    for((index=${#1}-1;index>=0;index--)); do rev="$rev${1:$index:1}"; done
    echo "$rev"
}

if [ "$#" -ne 1 ]; then
    echo "Usage: reverse_string.sh <string>"
    exit 1
fi

main "$@"