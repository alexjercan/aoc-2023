#!/bin/bash

input=$(cat ../input/day01.input)

echo "$input" \
    | sed -E 's/one/one1one/g' \
    | sed -E 's/two/two2two/g' \
    | sed -E 's/three/three3three/g' \
    | sed -E 's/four/four4four/g' \
    | sed -E 's/five/five5five/g' \
    | sed -E 's/six/six6six/g' \
    | sed -E 's/seven/seven7seven/g' \
    | sed -E 's/eight/eight8eight/g' \
    | sed -E 's/nine/nine9nine/g' \
    | tr -d '[a-z]' \
    | sed -E 's/^(.)(.*)$/\1\1\2/' \
    | sed -E 's/^(.).*(.)$/\1\2/' \
    | paste -sd+ \
    | bc
