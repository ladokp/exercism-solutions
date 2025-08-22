#!/usr/bin/env bash

function usage() {
    echo "Usage: $0 <year>"
    exit 1
}

(($# == 1)) || usage
year=$1

[[ $year =~ ^[0-9]+$ ]] || usage

(($year % 4 == 0 && $year % 100 != 0 || $year % 400 == 0)) && echo true || echo false