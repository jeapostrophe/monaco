#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <time.h>
#include <math.h>

typedef uint8_t action;
typedef uint8_t actor;

#define BIT(x,i) ((x) >> (i) & 1)
#define FLIP(x,i) ((x) ^ (1 << (i)))
#define SET(x,i) ((x) | (1 << (i)))
#define FIELD(x, l, h) (((1 << ((h)-(l)))-1) & ((x)>>(l)))
