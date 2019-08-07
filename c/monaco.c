action max_action;
bool terminal_p( state st );
actor winner( state st ); // 0 is draw
void render_st( state st );
bool decode_action( state st, char c, action *a );
bool legal_p( state st, action a );
actor who( state st );
state eval( state st, action a );

#define POOL_SIZE UINT16_MAX
typedef uint16_t node_ptr;
#define NULL_NODE ((node_ptr)0)

typedef struct {
  uint32_t  w; // wins (LSB is 0.5)
  uint32_t  v; // visits
  node_ptr lc; // left-child
  node_ptr rs; // right-sibling
  node_ptr pq; // prev in queue
  node_ptr nq; // next in queue
  action   ia; // initiating action
  action   na; // next action
  actor    wh; // who is acting
  bool     tr; // if terminal
} node;

node POOL[POOL_SIZE] = {{0}};
node_ptr free_ptr = 0;

action decide( node_ptr gt, state st ) {
  action a;
  do {
    a = rand() % max_action; }
  while ( ! legal_p( st, a ) );
  return a; }

node_ptr choose( node_ptr gt, action a ) {
  // XXX
  return NULL_NODE; }

void play() {
  state st = st0;
  node_ptr gt = NULL_NODE;

  while (1) {
    render_st(st);
    if ( terminal_p(st) ) {
      break; }
    
    action a;
    if ( who(st) == 1 ) {
      char c;
      do {
        printf("> ");
        scanf("%c", &c);;
      } while ( ! (decode_action(st, c, &a) && legal_p(st, a)) ); }
    else {
      a = decide(gt, st); }
    
    st = eval(st, a);
    gt = choose(gt, a); }

  actor w = winner(st);
  if ( w == 0 ) {
    printf("Draw\n"); }
  else {
    printf("Winner is %s\n", w == 1 ? "Player" : "Computer"); }
  return; }

int main() {
  srand(time(NULL));
  printf("%f\n", (((float)sizeof(POOL))/1024.0/1024.0));
  play();
  return 0; }
