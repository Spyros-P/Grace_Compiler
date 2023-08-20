#include <stdio.h>
#include <stdint.h>
#include "mylib.h"

void writeInteger(int32_t num) {
    printf("%d", num);
}

void writeChar(char c) {
    printf("%c", c);
}

void writeString(char *str) {
    printf("%s", str);
}

int32_t readInteger() {
    int32_t num;
    scanf("%d", &num);
    return num;
}

int8_t readChar() {
    int8_t c;
    scanf("%c", &c);
    return c;
}

void readString(int32_t size, char *str) {
    scanf("%s", str);
    str[size-1] = '\0';
}

int32_t ascii(int8_t c) {
    return (int32_t) c;
}

int8_t chr(int32_t n) {
    return (int8_t) n;
}

int32_t strlen(char *str) {
    int32_t counter = 0;
    while (str[counter] != '\0') {
        counter++;
    }
    return counter;
}

int32_t strcmp(char *str1, char *str2) {
    int32_t i = 0;
    while (str1[i] != '\0' && str2[i] != '\0') {
        if (str1[i] != str2[i]) {
            return str1[i] - str2[i];
        }
        i++;
    }
    return str1[i] - str2[i];
}

void strcpy(char *dest, char *src) {
    int32_t i = 0;
    while (src[i] != '\0') {
        dest[i] = src[i];
        i++;
    }
    dest[i] = '\0';
}

void strcat(char *dest, char *src) {
    int32_t i = 0;
    int32_t j = 0;
    while (dest[i] != '\0') {
        i++;
    }
    while (src[j] != '\0') {
        dest[i] = src[j];
        i++;
        j++;
    }
    dest[i] = '\0';
}
