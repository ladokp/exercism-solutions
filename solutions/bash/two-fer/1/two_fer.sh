#!/usr/bin/env bash

if [ "$#" -lt 1 ]; then
    echo "One for you, one for me."
    exit
fi

echo "One for $1, one for me."