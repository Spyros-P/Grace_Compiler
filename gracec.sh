#!/bin/bash

export grc="./main.native"

# if no flags are specified, the input is a source file # and the output is two files: .imm and .asm with the same name
# as the source file.

# if the -O flag is specified, produce optimized code
# if the -f flag is specified, the input is read from the standard input
# and the output .asm is written to the standard output.
# if the -i flag is specified, the input is read from the standard input
# and the output .imm is written to the standard output.

# Initialize an array to store flags
flags=()

# Iterate through the arguments and collect flags
for arg in "$@"; do
    case $arg in
        -O|-f|-i)
            flags+=("$arg")
            ;;
        -*)
            # Handle other arguments or display an error message
            echo "Unknown option: $arg"
            exit 1
            ;;
        *)
            filename="$arg" 
    esac
done

mainname=$(basename "$filename" | cut -d. -f1)
pathname=$(dirname "$filename")

mainname="$pathname/$mainname"

if [[ "${flags[@]}" =~ "-f" ]] || [[ "${flags[@]}" =~ "-i" ]] && [[ ${#filename} -gt 0 ]]; then
    echo "Error: source file cannot be specified when -f or -i is set."
    exit 1
fi

# Check if specific flags are set
if [[ "${flags[@]}" =~ "-f" ]] || [[ "${flags[@]}" =~ "-i" ]] ; then
    # read the input from standard input and store it in a temporary file
    # then pass the temporary file as input to the executable

    temp_file=$(mktemp)
    while IFS= read -r line || [[ -n "$line" ]];
    do
        echo "$line"
    done > "$temp_file"

    if [[ "${flags[@]}" =~ "-O" ]] ; then
        ${grc} "$temp_file" -O &&
        llc a.ll -o $temp_file.s -relocation-model=pic &&
        clang -o $temp_file.out $temp_file.s lib/libmylib.a &&
        mv a.ll $temp_file.ll
    else
        ${grc} "$temp_file" &&
        llc a.ll -o $temp_file.s -relocation-model=pic &&
        clang -o $temp_file.out $temp_file.s lib/libmylib.a &&
        mv a.ll $temp_file.ll
    fi
    if [[ "${flags[@]}" =~ "-f" ]] ; then
        # if -f is set, then the output .asm is written to the standard output.
        cat $temp_file.s
    fi
    if [[ "${flags[@]}" =~ "-i" ]] ; then
        # if -i is set, then the output .imm is written to the standard output.
        cat $temp_file.ll
    fi
    # cleanup
    rm "$temp_file" "$temp_file".ll "$temp_file".s
else
    # if neither -f nor -i is set, then the input is a source file
    # and the output is two files: .imm and .asm with the same name
    # as the source file.

    if [[ "${flags[@]}" =~ "-O" ]] ; then
        ${grc} "$filename" -O &&
        llc a.ll -o $mainname.asm -relocation-model=pic &&
        clang -o $mainname.out $mainname.asm lib/libmylib.a &&
        mv a.ll $mainname.imm
    else
        ${grc} "$filename" &&
        llc a.ll -o $mainname.s -relocation-model=pic &&
        clang -o $mainname.out $mainname.s lib/libmylib.a &&
        mv a.ll $mainname.imm
    fi
fi
