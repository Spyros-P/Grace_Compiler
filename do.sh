export grc="./main.native"

rm -f a.o mylib.o &&
make libmylib.a &&
mv libmylib.a .. &&
make &&
clear &&
${grc} test_programs/test.grc &&
cat a.ll &&
mv ../libmylib.a . &&
llc -filetype=obj a.ll -o a.o &&
clang a.o -o a.out -L. -lmylib