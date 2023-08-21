#ifndef MYLIB_H
#define MYLIB_H

void writeInteger(int32_t num);
void writeChar(int8_t c);
void writeString(int8_t *str);
int32_t readInteger();
int8_t readChar();
void readString(int32_t size, int8_t *str);
int32_t ascii(int8_t c);
int8_t chr(int32_t n);
int32_t strlen(int8_t *str);
int32_t strcmp(int8_t *str1, int8_t *str2);
void strcpy(int8_t *dest, int8_t *src);
void strcat(int8_t *dest, int8_t *src);



#endif
