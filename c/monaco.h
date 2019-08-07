#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <time.h>
#include <math.h>

typedef uint8_t action;
typedef uint8_t actor;

#define BIT(x,i) ((x) >> (uint64_t)(i) & (uint64_t)1)
// XXX ideally we'd have typeof here
#define FLIP(x,i) ((x) ^ ((uint64_t)1 << (uint64_t)(i)))
#define SET(x,i) ((x) | ((uint64_t)1 << (uint64_t)(i)))
#define FIELD(x, l, h) ((((uint64_t)1 << (uint64_t)((h)-(l)))-(uint64_t)1) & ((x)>>(uint64_t)(l)))

#define FIELDV(l, h, v) ((v) << (l))

#define MASK(len) ((1 << (len))-1)
#define FIELD_MASK(start, len) (MASK(len)<<(start))
#define FIELD_PREP(x, start, len) (((x)&MASK(len)) << (start))
#define FIELD_SET(x, l, h, v) (((x) &~ FIELD_MASK((l),((h)-(l)))) | FIELD_PREP((v), (l), ((h)-(l))))

#define max(x,y) ((x) > (y) ? (x) : (y))

bool decode_action_keys( const char *keys, action max_key, char c, action *a );

void display_bits(uint64_t b, uint8_t len) {
  for ( uint8_t i = 0; i < len; i++ ) {
    printf("%c", ( BIT(b,i) ? '1' : '0' )); }
  printf("\n");}
