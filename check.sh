export grc="./main.native"
export folder="./test_programs/error_programs"

for i in ${folder}/*
do
    echo ${grc} ${i}
    ${grc} ${i}
done