#!/bin/bash

export grc="./main.native"
export folder="./programs/*.grc"

temp_file=$(mktemp)

BLUE="\033[94m"
GREEN="\033[32m"
RED="\033[31m"
RESET="\033[0m"

for i in $folder
do
    command="$grc $i $temp_file.ll"
    echo -e "\n$BLUE$command$RESET\n"
    $command &&
    echo -e "\n${GREEN}Compiled sucecssfully!$RESET\n" &&
    llc $temp_file.ll -o $temp_file.s -relocation-model=pic &&
    clang -o $temp_file.out $temp_file.s lib/libmylib.a &&
    ($temp_file.out || echo -e "\n${RED}Exited:$RESET Error Code : $?\n")
done