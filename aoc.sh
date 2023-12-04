#!/bin/bash

Color_Off='\033[0m'

IRed='\033[0;91m'
IGreen='\033[0;92m'
IWhite='\033[0;97m'

BIGreen='\033[1;92m'

bash get

stack build

echo -e "${BIGreen}Advent of Code 2023${Color_Off}"
echo

echo -e "${IRed}--- Day 1: Trebuchet?! ---${Color_Off}"
stack exec day01 < input/day01.input

echo -e "${IWhite}--- Day 2: Cube Conundrum ---${Color_Off}"
stack exec day02 < input/day02.input

echo -e "${IGreen}--- Day 3: Gear Ratios ---${Color_Off}"
stack exec day03 < input/day03.input

echo -e "${IWhite}--- Day 4: Scratchcards ---${Color_Off}"
stack exec day04 < input/day04.input
