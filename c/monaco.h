#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <time.h>
#include <math.h>

typedef uint8_t action;
typedef uint8_t actor;

#define DEFINE_FIELD(name, start, len) \
  const state name##_START = start; \
  const state name##_LEN = len; \
  const state name##_STOP = (name##_START+name##_LEN)

#define BIT(n) \
  ( ((uint64_t)1<<(n)) )

#define BIT_SET(y, mask) \
  ( y |  (mask) )
#define BIT_CLEAR(y, mask) \
  ( y & ~(mask) )
#define BIT_FLIP(y, mask) \
  ( y ^  (mask) )

#define BIT_TESTi(y, i) \
  ( ! ! (y & (BIT(i))) )
#define BIT_FLIPi(y, i) \
  BIT_FLIP(y, BIT(i))
#define BIT_SETi(y, i) \
  BIT_SET(y, BIT(i))

#define BIT_MASK(len) \
  ( BIT(len)-1 )

#define BF_MASK(start, len) \
  ( BIT_MASK(len)<<(start) )

#define BF_PREP(x, start, len) \
  ( ((x)&BIT_MASK(len)) << (start) )

#define BF_GET(y, start, len) \
  ( ((y)>>(start)) & BIT_MASK(len) )

#define BF_SET(y, x, start, len) \
  ( ((y) &~ BF_MASK(start, len)) | BF_PREP(x, start, len) )

#define BFN_SET(y, x, name) BF_SET(y, x, name##_START, name##_LEN)
#define BFN_GET(y, name) BF_GET(y, name##_START, name##_LEN)

#define max(x,y) ((x) > (y) ? (x) : (y))

bool decode_action_keys( const char *keys, action max_key, char c, action *a );

void display_bits(uint64_t b, uint8_t len) {
  for ( uint8_t i = 0; i < len; i++ ) {
    printf("%c", ( BIT_TESTi(b,i) ? '1' : '0' )); }
  printf("\n");}

#define assert(c) _assert(c, __FILE__, __LINE__)
void _assert( bool c, const char *file, int line ) {
  if ( ! c ) {
    fprintf(stderr, "failed assertion on %s:L%d\n", file, line);
    exit(1); } }
