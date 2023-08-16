export grc="./main.native"

make lib/libmylib.a &&
make &&
clear &&
${grc} $1 &&
cat a.ll &&
llc a.ll -o a.s -relocation-model=pic &&
clang -o a.out a.s lib/libmylib.a