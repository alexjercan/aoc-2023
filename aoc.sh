#!/bin/bash

Color_Off='\033[0m'

IRed='\033[0;91m'
IGreen='\033[0;92m'
IYellow='\033[0;93m'

BIGreen='\033[1;92m'

bash get

stack build

echo -e "${BIGreen}Advent of Code 2023${Color_Off}"

star="\e[5;33m*\e[0m\e[1;32m"
o="\e[0m\e[1;31mo\e[0m\e[1;32m"

echo -e "
        ${star}
       /.\\
      /${o}..\\
      /..${o}\\
     /.${o}..${o}\\
     /...${o}.\\
    /..${o}....\\
    ^^^[_]^^^
"

echo -e "${IRed}--- Day 1: Trebuchet?! ---${Color_Off}"
stack exec day01 < input/day01.input

echo -e "${IYellow}--- Day 2: Cube Conundrum ---${Color_Off}"
stack exec day02 < input/day02.input

echo -e "${IGreen}--- Day 3: Gear Ratios ---${Color_Off}"
stack exec day03 < input/day03.input

echo -e "${IYellow}--- Day 4: Scratchcards ---${Color_Off}"
stack exec day04 < input/day04.input

echo -e "${IRed}--- Day 5: If You Give A Seed A Fertilizer ----${Color_Off}"
stack exec day05 < input/day05.input

echo -e "${IYellow}--- Day 6: Wait For It ---${Color_Off}"
stack exec day06 < input/day06.input

echo -e "${IGreen}--- Day 7: Camel Cards ---${Color_Off}"
stack exec day07 < input/day07.input

echo -e "${IRed}--- Day 8: Haunted Wasteland ---${Color_Off}"
stack exec day08 < input/day08.input

echo -e "${IYellow}--- Day 9: Mirage Maintenance ---${Color_Off}"
stack exec day09 < input/day09.input
