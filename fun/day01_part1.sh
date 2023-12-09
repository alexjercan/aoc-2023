#!/bin/bash

input=$(cat ../input/day01.input)

echo "$input" | tr -d '[a-z]' | sed -E 's/^(.)(.*)$/\1\1\2/' | sed -E 's/^(.).*(.)$/\1\2/' | paste -sd+ | bc
