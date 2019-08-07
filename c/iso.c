#include "monaco.h"

#define DIRS 9
#define ROWS 6
#define COLS 8
uint8_t cell( uint8_t r, uint8_t c ) {
  return c + r * COLS; }

action how_many_actions = max(DIRS,max(ROWS,COLS));

typedef uint64_t state;

#define PLAYER_IDX 0
#define PHASE_START (PLAYER_IDX+1)
#define PHASE_STOP (PHASE_START+3)
#define P0R_START (PHASE_STOP+1)
#define P0R_STOP (P0R_START+3)
#define P0C_START (P0R_STOP+1)
#define P0C_STOP (P0C_START+3)
#define P1R_START (P0C_STOP+1)
#define P1R_STOP (P1R_START+3)
#define P1C_START (P1R_STOP+1)
#define P1C_STOP (P1C_START+3)
#define BOARD_START (P1C_STOP+1)
#define BOARD_STOP (BOARD_START+(ROWS*COLS))
#define BOARD_CELL(r, c) (BOARD_START+cell(r,c))

typedef struct {
  int8_t r;
  int8_t c; } posn;
posn posn_plus ( posn px, posn py ) {
  px.r += py.r;
  px.c += py.c;
  return px; }

#define PHASE_MOVE 0
#define PHASE_ROW 1
#define PHASE_COL(x) (2+(x))
typedef uint8_t phase_t;
#define phase(st) FIELD(st, PHASE_START, PHASE_STOP)

typedef uint64_t board_t;

state st0 =
    0
  | FIELDV(PHASE_START, PHASE_STOP, PHASE_MOVE)
  | FIELDV(P0R_START, P0R_STOP, 2)
  | FIELDV(P0C_START, P0C_STOP, 0)
  | FIELDV(P1R_START, P1R_STOP, 3)
  | FIELDV(P1C_START, P1C_STOP, COLS-1);

actor who ( state st ) {
  return BIT(st,PLAYER_IDX) ? 2 : 1; }

posn p0_posn(state st) {
  posn p = { .r = FIELD(st, P0R_START, P0R_STOP),
             .c = FIELD(st, P0C_START, P0C_STOP) };
  return p; }
posn p1_posn(state st) {
  posn p = { .r = FIELD(st, P1R_START, P1R_STOP),
             .c = FIELD(st, P1C_START, P1C_STOP) };
  return p; }
#define PwR_START(st) ((who(st) == 1) ? P0R_START : P1R_START)
#define PwR_STOP(st) ((who(st) == 1) ? P0R_STOP : P1R_STOP)
#define PwC_START(st) ((who(st) == 1) ? P0C_START : P1C_START)
#define PwC_STOP(st) ((who(st) == 1) ? P0C_STOP : P1C_STOP)
posn pW_posn(state st) {
  posn p = { .r = FIELD(st, PwR_START(st), PwR_STOP(st)),
             .c = FIELD(st, PwC_START(st), PwC_STOP(st)) };
  return p; }

board_t board(state st) {
  board_t b = FIELD(st, BOARD_START, BOARD_STOP);
  posn p0 = p0_posn(st); b = SET(b, cell(p0.r, p0.c));
  posn p1 = p1_posn(st);  b = SET(b, cell(p1.r, p1.c));
  return b; }

actor winner ( state st ) {
  return BIT(st,PLAYER_IDX) ? 1 : 2; }

const char dir_keys[] = "qweasdzxc";
const char row_keys[] = "qwerty";
const char col_keys[] = "asdfghjk";
bool decode_action( state st, char c, action *a ) {
  phase_t p = phase(st);
  if ( p == PHASE_MOVE ) {
    return decode_action_keys( dir_keys, DIRS, c, a ); }
  else if ( p == PHASE_ROW ) {
    return decode_action_keys( row_keys, ROWS, c, a ); }
  else {
    return decode_action_keys( col_keys, COLS, c, a ); } }

posn decode_move( action a ) {
  posn p = { .r = a / 3 - 1,
             .c = a % 3 - 1 };
  return p; }

bool legal_p( state st, action a ) {
  phase_t p = phase(st);
  board_t b = board(st);

  if ( p == PHASE_MOVE ) {
    if ( a >= DIRS ) { return false; }
    posn cp = pW_posn(st);
    posn mp = decode_move(a);
    posn np = posn_plus(cp, mp);
    return
      (0 <= np.r) &&
      (0 <= np.c) &&
      (np.r < ROWS) &&
      (np.c < COLS) &&
      (! BIT(b, cell(np.r, np.c)) ); }
  else if ( p == PHASE_ROW ) {
    if ( a >= ROWS ) { return false; }
    for ( uint8_t c = 0; c < COLS; c++ ) {
      if ( ! BIT(b, cell(a, c)) ) {
        return true; } }
    return false; }
  else {
    if ( a >= COLS ) { return false; }
    uint8_t r = p - PHASE_COL(0);
    return ! BIT(b, cell(r, a)); } }

bool terminal_p( state st ) {
  if ( phase(st) != PHASE_MOVE ) { return false; }
  for ( uint8_t d = 0; d < DIRS; d++ ) {
    if ( legal_p(st, d) ) { return false; } }
  return true; }

void render_st( state st ) {
  phase_t p = phase(st);
  posn p0 = p0_posn(st);
  posn p1 = p1_posn(st);
  printf("\n");
  display_bits(st, BOARD_STOP);
  printf(" %s\n", col_keys);
  for ( uint8_t r = 0; r < ROWS; r++ ) {
    printf("%c", row_keys[r]);
    for ( uint8_t c = 0; c < COLS; c++ ) {
      printf("%c", (BIT(st, BOARD_CELL(r,c)) ? '#'
                    : ((p0.r == r && p0.c == c) ? '0'
                       : (p1.r == r && p1.c == c) ? '1'
                       : '.'))); }
    printf("\n"); }
  printf("%d's turn: ", (who(st)-1) );
  if ( p == PHASE_MOVE ) {
    printf("move"); }
  else if ( p == PHASE_ROW ) {
    printf("select row"); }
  else {
    printf("select col from row %c",
           row_keys[(p - PHASE_COL(0))]); }
  printf("\n");
  return; }

state eval( state st, action a ) {
  phase_t p = phase(st);
  if ( p == PHASE_MOVE ) {
    posn cp = pW_posn(st);
    posn mp = decode_move(a);
    posn np = posn_plus(cp, mp);
    printf("m0"); render_st(st);
    st = FIELD_SET(st, PHASE_START, PHASE_STOP, PHASE_ROW);
    printf("m1"); render_st(st);
    st = FIELD_SET(st, PwR_START(st), PwR_STOP(st), np.r);
    printf("m2"); render_st(st);
    st = FIELD_SET(st, PwC_START(st), PwC_STOP(st), np.c);
    printf("m3"); render_st(st);
    return st; }
  else if ( p == PHASE_ROW ) {
    printf("pr0"); render_st(st);
    st = FIELD_SET(st, PHASE_START, PHASE_STOP, PHASE_COL(a));
    printf("pr1"); render_st(st);
    return st; }
  else {
    uint8_t r = p - PHASE_COL(0);
    printf("pc0"); render_st(st);
    st = FLIP(st, PLAYER_IDX);
    printf("pc1"); render_st(st);
    st = FIELD_SET(st, PHASE_START, PHASE_STOP, PHASE_MOVE);
    printf("pc2"); render_st(st);
    st = SET(st, BOARD_CELL(r, a));
    printf("pc3"); render_st(st);
    exit(1);
    return st; } }
  
#include "monaco.c"
