export grc="./main.native"

# if no flags are specified, the input is a source file
# and the output is two files: .imm and .asm with the same name
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
    *)
      # Handle other arguments or display an error message
      echo "Unknown option: $arg"
      exit 1
      ;;
  esac
done

flag_O = [[ " ${flags[@]} " =~ " -O " ]]
flag_f = [[ " ${flags[@]} " =~ " -f " ]]
flag_i = [[ " ${flags[@]} " =~ " -i " ]]

if flag_f || flag_i && [[ $# -gt 0 ]]; then
    echo "Error: source file cannot be specified when -f or -i is set."
    exit 1
fi

# Check if specific flags are set
if flag_f || flag_i; then
    # read the input from standard input and store it in a temporary file
    # then pass the temporary file as input to the executable
    temp_file=$(mktemp)
    cat > "$temp_file"
    ${grc} "$temp_file" "${flags[@]}" &&
    llc a.ll -o a.s -relocation-model=pic &&
    clang -o a.out a.s lib/libmylib.a &&
    if flag_f ; then
        # if -f is set, then the output .asm is written to the standard output.
        cat a.s
    fi
    if flag_i ; then
        # if -i is set, then the output .imm is written to the standard output.
        cat a.ll
    fi
    rm a.ll a.s "$temp_file"
else
    # if neither -f nor -i is set, then the input is a source file
    # and the output is two files: .imm and .asm with the same name
    # as the source file.

    # get the name of the source file
    filename=$(basename -- "$1")
    ${grc} $@ &&
    llc a.ll -o $filename.s -relocation-model=pic &&
    clang -o $filename.out $filename.s lib/libmylib.a &&
    mv a.ll $filename.ll
fi

