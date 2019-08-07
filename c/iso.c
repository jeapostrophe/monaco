#include "monaco.h"

#define DIRS 9
#define ROWS 6
#define COLS 8
uint8_t cell( uint8_t r, uint8_t c ) {
  return c + r * COLS; }

action how_many_actions = max(DIRS,max(ROWS,COLS));

typedef uint64_t state;

DEFINE_FIELD(PLAYER, 0, 1);
DEFINE_FIELD(PHASE, PLAYER_STOP, 3);
#define PR_LEN 3
#define PC_LEN 3
DEFINE_FIELD(P0R, PHASE_STOP, PR_LEN);
DEFINE_FIELD(P0C, P0R_STOP, PC_LEN);
DEFINE_FIELD(P1R, P0C_STOP, PR_LEN);
DEFINE_FIELD(P1C, P1R_STOP, PC_LEN);
DEFINE_FIELD(BOARD, P1C_STOP, (ROWS*COLS));
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
#define phase(st) BFN_GET(st, PHASE)

typedef uint64_t board_t;

state st0 =
    0
  | BFN_SET(0, PHASE_MOVE, PHASE)
  | BFN_SET(0, 2, P0R)
  | BFN_SET(0, 0, P0C)
  | BFN_SET(0, 3, P1R)
  | BFN_SET(0, COLS-1, P1C);

actor who ( state st ) {
  return BFN_GET(st, PLAYER) ? 2 : 1; }

posn p0_posn(state st) {
  posn p = { .r = BFN_GET(st, P0R),
             .c = BFN_GET(st, P0C) };
  return p; }
posn p1_posn(state st) {
  posn p = { .r = BFN_GET(st, P1R),
             .c = BFN_GET(st, P1C) };
  return p; }
posn pW_posn(state st) {
  return who(st) == 1 ? p0_posn(st) : p1_posn(st); }

board_t board(state st) {
  board_t b = BFN_GET(st, BOARD);
  posn p0 = p0_posn(st); b = BIT_SETi(b, cell(p0.r, p0.c));
  posn p1 = p1_posn(st);  b = BIT_SETi(b, cell(p1.r, p1.c));
  return b; }

actor winner ( state st ) {
  return BFN_GET(st,PLAYER) ? 1 : 2; }

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
      (! BIT_TESTi(b, cell(np.r, np.c)) ); }
  else if ( p == PHASE_ROW ) {
    if ( a >= ROWS ) { return false; }
    for ( uint8_t c = 0; c < COLS; c++ ) {
      if ( ! BIT_TESTi(b, cell(a, c)) ) {
        return true; } }
    return false; }
  else {
    if ( a >= COLS ) { return false; }
    uint8_t r = p - PHASE_COL(0);
    return ! BIT_TESTi(b, cell(r, a)); } }

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

  printf(" %s\n", col_keys);
  for ( uint8_t r = 0; r < ROWS; r++ ) {
    printf("%c", row_keys[r]);
    for ( uint8_t c = 0; c < COLS; c++ ) {
      printf("%c", (BIT_TESTi(st, BOARD_CELL(r,c)) ? '#'
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

#define PwR_START(st) ((who(st) == 1) ? P0R_START : P1R_START)
#define PwC_START(st) ((who(st) == 1) ? P0C_START : P1C_START)
state eval( state st, action a ) {
  phase_t p = phase(st);
  if ( p == PHASE_MOVE ) {
    posn cp = pW_posn(st);
    posn mp = decode_move(a);
    posn np = posn_plus(cp, mp);
    st = BFN_SET(st, PHASE_ROW, PHASE);
    st = BF_SET(st, np.r, PwR_START(st), PR_LEN);
    st = BF_SET(st, np.c, PwC_START(st), PC_LEN);
    return st; }
  else if ( p == PHASE_ROW ) {
    st = BFN_SET(st, PHASE_COL(a), PHASE);
    return st; }
  else {
    uint8_t r = p - PHASE_COL(0);
    st = BFN_SET(st, !BFN_GET(st, PLAYER), PLAYER);
    st = BFN_SET(st, PHASE_MOVE, PHASE);
    st = BIT_SETi(st, BOARD_CELL(r, a));
    return st; } }
  
#include "monaco.c"
