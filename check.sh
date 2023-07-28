export grc="./main.native"

for i in programs/*
do
    echo ${grc} ${i}
    ${grc} ${i}
done