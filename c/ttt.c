#include "monaco.h"

typedef uint32_t state;
typedef uint16_t board;

#define ROWS 3
#define COLS 3
#define SLOTS (ROWS*COLS)

#define PLAYER_IDX 0
#define O_START (PLAYER_IDX+1)
#define O_LEN SLOTS
#define X_START (O_START+O_LEN+1)
#define X_LEN SLOTS

state st0 = 0;

board marks_x( state st ) {
  return BF_GET(st, X_START, X_LEN); }
board marks_o( state st ) {
  return BF_GET(st, O_START, O_LEN); }
board marks_all( state st ) {
  return marks_x(st) | marks_o(st); }

uint8_t cell( uint8_t r, uint8_t c ) {
  return c + r * COLS; }
bool seq( board b, uint8_t r, uint8_t c, uint8_t dr, uint8_t dc ) {
  return BIT_TESTi(b, cell(r,c))
    && BIT_TESTi(b, cell(r+dr,c+dc))
    && BIT_TESTi(b, cell(r+dr+dr,c+dc+dc)); }
bool col( board b, uint8_t r ) {
  return seq(b, r, 0, 0, +1); }
bool row( board b, uint8_t c ) {
  return seq(b, 0, c, +1, 0); }
bool winning_p( board b ) {
  return
    row(b, 0) || row(b, 1) || row(b, 2) ||
    col(b, 0) || col(b, 1) || col(b, 2) ||
    seq(b, 0, 0, +1, +1) ||
    seq(b, 0, 2, +1, -1); }

bool complete_p( board b ) {
  return b == 0x1ff; }

void analyze( state st, bool *t, actor *w ) {
  *t = true;
  if ( winning_p(marks_x(st)) ) {
    *w = 2; return; }
  else if ( winning_p(marks_o(st)) ) {
    *w = 1; return; }
  else if ( complete_p(marks_all(st)) ) {
    *w = 0; return; }
  *t = false;
  return; }

bool terminal_p( state st ) {
  bool t; actor w;
  analyze( st, &t, &w );
  return t; }

actor winner( state st ) {
  bool t; actor w;
  analyze( st, &t, &w );
  return w; }

actor who( state st ) {
  return 1 + BIT_TESTi(st, PLAYER_IDX); }

action estimate_legal( state st ) {
  return SLOTS; }
bool legal_p( state st, action a ) {
  return (BIT_TESTi(marks_all(st), a) == 0); }

const char keys[] = "qweasdzxc";
bool decode_action( state st, char c, action *a ) {
  return decode_action_keys( keys, SLOTS, c, a ); }

state eval( state st, action a ) {
  uint8_t me = who(st) == 1 ? O_START : X_START;
  st = BIT_FLIPi(st, PLAYER_IDX);
  st = BIT_SETi(st, a + me);
  return st; }

void render_st( state st ) {
  board xm = marks_x(st);
  board om = marks_o(st);
  printf("\n");
  for ( uint8_t r = 0; r < ROWS; r++ ) {
    for ( uint8_t c = 0; c < COLS; c++ ) {
      uint8_t i = cell(r, c);
      printf("%c", (BIT_TESTi(xm, i) ? 'X' : (BIT_TESTi(om, i) ? 'O' : ' ')) );
      if ( c != COLS-1 ) { printf("│"); } }
    printf("\n");
    if ( r != ROWS-1 ) {
      printf ("─┼─┼─\n"); } }
  printf("%c's turn\n", (who(st) == 1 ? 'O' : 'X') ); }

#include "monaco.c"
