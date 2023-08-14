#include <stdio.h>
#include <stdint.h>
#include "mylib.h"

void writeInteger(int32_t num) {
    printf("%d\n", num);
}

void writeString(char *str) {
    printf("%s\n", str);
}

int32_t readInteger() {
    int32_t num;
    scanf("%d", &num);
    return num;
}

int32_t strlen(char *str) {
    int32_t counter = 0;
    while (str[counter] != '\0') {
        counter++;
    }
    return counter;
}
