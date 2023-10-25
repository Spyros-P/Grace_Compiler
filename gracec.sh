#!/bin/bash

export grc="./main.native"

# if no flags are specified, the input is a source file # and the output is two files: .imm and .asm with the same name
# as the source file.

# if the -O flag is specified, produce optimized code
# if the -f flag is specified, the input is read from the standard input
# and the output .asm is written to the standard output.
# if the -i flag is specified, the input is read from the standard input
# and the output .imm is written to the standard output.

RED='\033[0;31m'
RESET='\033[0m'
ERROR="${RED}Error:${RESET}"

# Initialize an array to store flags
flags=()
filename_count=0

# Iterate through the arguments and collect flags
for arg in $@; do
    case $arg in
        -O|-f|-i)
            flags+=($arg)
            ;;
        -*)
            # Handle other arguments or display an error message
            echo -e "${ERROR} Unknown option: $arg"
            exit 1
            ;;
        *)
            filename=$arg
            ((filename_count++))
            if [ $filename_count -gt 1 ]; then
                echo -e "${ERROR} More than one filename was given"
                exit 1
            fi
            ;;
    esac
done


if [[ ${flags[@]} =~ "-f" ]] || [[ ${flags[@]} =~ "-i" ]] && [[ ${#filename} -gt 0 ]]; then
    echo -e "${ERROR} Source file cannot be specified when -f or -i is set."
    exit 1
fi

# Check if specific flags are set
if [[ ${flags[@]} =~ "-f" ]] || [[ ${flags[@]} =~ "-i" ]] ; then
    # read the input from standard input and store it in a temporary file
    # then pass the temporary file as input to the executable

    temp_file=$(mktemp)
    while IFS= read -r line || [[ -n $line ]];
    do
        echo $line
    done > $temp_file

    if [[ ${flags[@]} =~ "-O" ]] ; then
        ${grc} $temp_file $temp_file.ll -O &&
        llc $temp_file.ll -o $temp_file.s -relocation-model=pic &&
        clang -o $temp_file.out $temp_file.s lib/libmylib.a
    else
        ${grc} $temp_file $temp_file.ll &&
        llc $temp_file.ll -o $temp_file.s -relocation-model=pic &&
        clang -o $temp_file.out $temp_file.s lib/libmylib.a
    fi
    if [[ ${flags[@]} =~ "-i" ]] ; then
        # if -i is set, then the output .imm is written to the standard output.
        cat $temp_file.ll
        if [[ ${flags[@]} =~ "-f" ]] ; then
            echo -e "\n========================================================================\n"
        fi
    fi
    if [[ ${flags[@]} =~ "-f" ]] ; then
        # if -f is set, then the output .asm is written to the standard output.
        cat $temp_file.s
    fi
    # cleanup
    rm -f $temp_file $temp_file.ll $temp_file.s
else
    # if neither -f nor -i is set, then the input is a source file
    # and the output is two files: .imm and .asm with the same name
    # as the source file.

    mainname=$(basename $filename | awk -F. 'NF==1 || ($1 == "" && NF == 2) {print; next} {OFS="."; NF--; print $0}')
    pathname=$(dirname $filename)

    mainname="$pathname/$mainname"

    if [[ ${flags[@]} =~ "-O" ]] ; then
        $grc $filename $mainname.imm -O &&
        llc $mainname.imm -o $mainname.asm -relocation-model=pic &&
        clang -o $mainname.out $mainname.asm lib/libmylib.a
    else
        $grc $filename $mainname.imm &&
        llc $mainname.imm -o $mainname.asm -relocation-model=pic &&
        clang -o $mainname.out $mainname.asm lib/libmylib.a
    fi
fi
