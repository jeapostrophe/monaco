#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>

typedef uint8_t action;
typedef uint8_t actor;

#define BIT(x,i) ((x) >> (i) & 1)
#define FLIP(x,i) ((x) ^ (1 << (i)))
#define SET(x,i) ((x) | (1 << (i)))
#define FIELD(x, l, h) (((1 << ((h)-(l)))-1) & ((x)>>(l)))
