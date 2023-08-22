export grc="./main.native"
export folder="./test_programs"

make && clear &&

for i in ${folder}/matmult*.grc
do
    command="${grc} ${i}"
    echo "\n\033[94m${command}\033[0m\n"
    ${command} &&
    echo "\n\033[32mCompiled sucecssfully!\033[0m\n" &&
    llc a.ll -o a.s -relocation-model=pic &&
    clang -o a.out a.s lib/libmylib.a &&
    ./a.out < ${folder}/matmult.txt
done