export grc="./main.native"

${grc} $1 &&
llc a.ll -o a.s -relocation-model=pic &&
clang -o a.out a.s lib/libmylib.a