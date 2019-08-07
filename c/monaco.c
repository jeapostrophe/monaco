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
  node_ptr pr; // parent
  node_ptr lc; // left-child
  node_ptr rs; // right-sibling
  node_ptr pq; // prev in queue
  node_ptr nq; // next in queue
  action   ia; // initiating action
  action   na; // next action to expand
  actor    wh; // who is acting
} node;

node NODE[POOL_SIZE] = {{0}};
node_ptr free_ptr = 0;

void initialize_pool() {
  node_ptr last = 0;
  for ( node_ptr n = 1; n < POOL_SIZE; n++ ) {
    NODE[n].pq = last;
    NODE[n].nq = n+1;
    last = n;}
  NODE[last].nq = 0;
  free_ptr = 1; }

uint64_t current_ms() {
  return clock_gettime_nsec_np(CLOCK_MONOTONIC_RAW) / 1000; }

node_ptr alloc_node( node_ptr parent, action ia, state st ) {
  fprintf(stderr, "XXX alloc_node\n");
  exit(1);
  return NULL_NODE; }

void free_node( node_ptr n ) {
  fprintf(stderr, "XXX free_node\n");
  exit(1);
  return; }

node_ptr select( node_ptr parent, uint8_t explore ) {
  double best_score = (-1.0/0.0);
  node_ptr best_child = NULL_NODE;

  double tlpv = 2.0 * log((double)NODE[parent].v);
  for ( node_ptr c = NODE[parent].lc; c != NULL_NODE; c = NODE[c].rs ) {
    double vd = ((double)NODE[c].v);
    double score =
      (((double)NODE[c].w) / vd)
      + explore * (sqrt(tlpv / vd));
    if ( score > best_score ) {
      best_score = score;
      best_child = c; } }

  return best_child; }

action next_legal( state st, action prev ) {
  do { prev--; }
  while ( prev >= 0 || ! legal_p(st, prev) );
  return prev; }

action decide( node_ptr gt, state st ) {
  uint32_t iters = 0;
  uint64_t deadline = current_ms() + 100;
  do {
    iters++;
    state sti = st;
    node_ptr gti = gt;

    // Tree Policy
    while ( NODE[gti].na == 0 && NODE[gti].lc != NULL_NODE ) {
      gti = select(gti, 1);
      sti = eval(sti, NODE[gti].ia); }

    // Expand
    if ( ! (NODE[gti].na == 0) ) {
      action m = NODE[gti].na - 1;
      NODE[gti].na = 1 + next_legal( sti, m );
      sti = eval(sti, m);
      gti = alloc_node( gti, m, sti ); }

    // Default Policy
    while ( ! terminal_p(sti) ) {
      action a;
      do { a = rand() % max_action; }
      while ( ! legal_p( sti, a ) );
      sti = eval(sti, a); }
    
    // Back propagate
    actor w = winner(sti);
    while ( gti != NULL_NODE ) {
      NODE[gti].w +=
        NODE[gti].wh == w ? 2 :
        (w == 0 ? 1 : 0);
      NODE[gti].v++;
      gti = NODE[gti].pr; }
    
  } while ( current_ms() < deadline );

  printf("Took %d steps", iters);
  node_ptr bc = select(gt, 0);
  return NODE[bc].ia; }

node_ptr choose( node_ptr gt, action a ) {
  if ( gt == NULL_NODE ) { return gt; }

  node_ptr r = NULL_NODE;
  node_ptr c = NODE[gt].lc;
  free_node(gt);
  while ( c != NULL_NODE ) {
    node_ptr t = c;
    c = NODE[t].rs;
    NODE[t].pr = NULL_NODE;
    if ( NODE[t].ia == a ) {
      r = t; }
    else {
      free_node(t); } }

  return r; }

void play() {
  state st = st0;
  node_ptr gt = NULL_NODE;

  while (1) {
    gt = gt != NULL_NODE ? gt : alloc_node( NULL_NODE, 0, st );
    printf("Expected value is %f\n",
           (((double)NODE[gt].w) / ((double)NODE[gt].v)));
    
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
  printf("Node is %4lu  B\n", sizeof(node));
  printf("Pool is %4lu KB\n", ((sizeof(NODE))/1024));
  initialize_pool();
  play();
  return 0; }
