// Interface to games

bool terminal_p( state st );
actor winner( state st ); // 0 is draw
void render_st( state st );
bool decode_action( state st, char c, action *a );
action estimate_legal( state st );
bool legal_p( state st, action a );
actor who( state st );
state eval( state st, action a );

// Monaco

const bool STUNTING = false;
const bool debug_p = false;

bool decode_action_keys( const char *keys, action max_key, char c, action *a ) {
  for ( action i = 0; i < max_key; i++ ) {
    if ( keys[i] == c ) {
      *a = i;
      return true; } }
  return false; }

// XXX It would be nice to be able to easily have N-bit numbers so I
// could have a 20-bit pointer. (Stored as 16, plus half a byte
// (shared with other nibbles)
#define POOL_SIZE 16 // (1*UINT16_MAX)
typedef uint16_t node_ptr;
#define NULL_NODE ((node_ptr)0)

typedef struct {
  float     w; // wins
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
node_ptr free_ptr = NULL_NODE;
node_ptr node_count = 0;
node_ptr theta_head = NULL_NODE;

void theta_insert( node_ptr x ) {
  assert( NODE[x].nq == NULL_NODE );
  assert( NODE[x].pq == NULL_NODE );

  if ( theta_head == NULL_NODE ) {
    NODE[x].pq = x;
    NODE[x].nq = x; }
  else {
    assert( NODE[theta_head].nq != NULL_NODE );
    assert( NODE[theta_head].pq != NULL_NODE );

    NODE[x].nq = theta_head;
    NODE[x].pq = NODE[theta_head].pq;
    NODE[NODE[x].pq].nq = x;
    NODE[theta_head].pq = x; }

  assert( NODE[x].nq != NULL_NODE );
  assert( NODE[x].pq != NULL_NODE );

  theta_head = x; }

void theta_remove( node_ptr x ) {
  node_ptr nq = NODE[x].nq;
  node_ptr pq = NODE[x].pq;
  NODE[x].nq = NULL_NODE;
  NODE[x].pq = NULL_NODE;

  if ( nq == x ) {
    assert( pq == x );
    assert( theta_head == x );
    theta_head = NULL_NODE; }
  else {
    NODE[pq].nq = nq;
    NODE[nq].pq = pq;
    if ( theta_head == x ) {
      theta_head = nq; } }

  assert( NODE[x].nq == NULL_NODE );
  assert( NODE[x].pq == NULL_NODE ); }

void theta_reinsert( node_ptr x ) {
  theta_remove(x);
  theta_insert(x); }

void dg_edge(FILE *g, node_ptr x, node_ptr y, bool same) {
  if ( y != NULL_NODE ) {
    fprintf(g, "  n%d -> n%d\n", x, y);
    if ( same ) {
      fprintf(g, "  { rank = same; n%d; n%d; }\n", x, y); } } }
void dump_graph(node_ptr curr) {
  FILE *g = fopen("graph.dot", "w");
  if ( ! g ) { perror("dump_graph"); exit(1); }

  fprintf(g, "strict digraph MCTS {\n");
  fprintf(g, "  rankdir = TB;\n");

  if ( free_ptr != NULL_NODE ) {
    fprintf(g, "  free [ shape = plaintext, label = \"free\" ]\n");
    fprintf(g, "  edge [ color = yellow ]\n");
    fprintf(g, "  free -> n%d\n", free_ptr); }

  for ( node_ptr n = 1; n < POOL_SIZE; n++ ) {
    fprintf(g, "  n%d [ shape = %s, label = \"%d\\n%2.2f\" ]\n",
            n, (n == curr ? "hexagon" : (NODE[n].wh == 1 ? "circle" : "square")),
            NODE[n].ia, (100.0 * NODE[n].w / NODE[n].v)); }

  fprintf(g, "  edge [ color = blue ]\n");
  for ( node_ptr n = 1; n < POOL_SIZE; n++ ) {
    dg_edge(g, n, NODE[n].pr, false); }

  fprintf(g, "  edge [ color = red ]\n");
  for ( node_ptr n = 1; n < POOL_SIZE; n++ ) {
    dg_edge(g, n, NODE[n].lc, false); }

  fprintf(g, "  edge [ color = green ]\n");
  for ( node_ptr n = 1; n < POOL_SIZE; n++ ) {
    dg_edge(g, n, NODE[n].rs, true); }

  // XXX The edges sometimes overlap
  if ( theta_head != NULL_NODE ) {
    fprintf(g, "  q  [ shape = plaintext, label = \"Θ\" ]\n");
    fprintf(g, "  edge [ color = orange ]\n");
    fprintf(g, "  q -> n%d\n", theta_head);
    for ( node_ptr n = 1; n < POOL_SIZE; n++ ) {
      dg_edge(g, n, NODE[n].nq, false); }
    /*
    fprintf(g, "  edge [ color = violet ]\n");
    for ( node_ptr n = 1; n < POOL_SIZE; n++ ) {
      dg_edge(g, n, NODE[n].pq, false); }
    */ }

  fprintf(g, "}\n");
  
  fclose(g); }
  
void initialize_pool() {
  node_ptr last = NULL_NODE;
  for ( node_ptr n = 1; n < POOL_SIZE; n++ ) {
    NODE[n].rs = n+1;
    last = n; }
  NODE[last].rs = NULL_NODE;
  free_ptr = 1; }

double current_ms() {
  return ((double)clock_gettime_nsec_np(CLOCK_MONOTONIC_RAW))
    / ((double)1000000); }

action next_legal( state st, action prev ) {
  while (1) {
    if ( prev == 0 ) {
      return 0; }
    else {
      prev--;
      if ( legal_p(st, prev) ) {
        return prev + 1; } } } }

node_ptr alloc_node( node_ptr parent, actor lastp, action ia, state st ) {
  node_count++;
  node_ptr new = free_ptr;
  if ( new == NULL_NODE ) {
    fprintf(stderr, "alloc_node: Out of memory\n");
    // XXX implement recycling
    dump_graph(parent);
    exit(1); }
  
  free_ptr = NODE[new].rs;

  NODE[new].w = 0.0;
  NODE[new].v = 0;
  NODE[new].pr = parent;
  NODE[new].lc = NULL_NODE;
  NODE[new].rs = NULL_NODE;
  NODE[new].ia = ia;
  if ( terminal_p(st) ) {
    NODE[new].na = 0;
  } else {
    action a = next_legal( st, estimate_legal( st ) );
    NODE[new].na = a; }
  NODE[new].wh = lastp;

  if ( parent != NULL_NODE ) {
    NODE[new].rs = NODE[parent].lc;
    NODE[parent].lc = new; }

  theta_insert(new);
  
  return new; }

void free_node( node_ptr n ) {
  node_count--;

  if ( NODE[n].pr != NULL_NODE ) {
    fprintf(stderr, "free_node: disconnect parent first\n");
    exit(1); }
  if ( NODE[n].lc != NULL_NODE ) {
    fprintf(stderr, "Free children (or disconnect) first\n");
    exit(1); }
  if ( NODE[n].rs != NULL_NODE ) {
    fprintf(stderr, "Free siblings (or disconnect) first\n");
    exit(1); }

  NODE[n].rs = free_ptr;
  free_ptr = n;

  theta_remove(n);

  return; }

void free_node_rec( node_ptr n ) {
  if ( n == NULL_NODE ) { return; }
  free_node_rec(NODE[n].lc); NODE[n].lc = NULL_NODE;
  free_node_rec(NODE[n].rs); NODE[n].rs = NULL_NODE;
  NODE[n].pr = NULL_NODE;
  free_node(n); }

node_ptr select( node_ptr parent, float explore_factor ) {
  double best_score = (-1.0/0.0);
  node_ptr best_child = NULL_NODE;

  double tlpv = log((double)NODE[parent].v);
  for ( node_ptr c = NODE[parent].lc;
        c != NULL_NODE;
        c = NODE[c].rs ) {
    double vd = ((double)NODE[c].v);
    double w = NODE[c].w;
    double explore_score = (sqrt(tlpv / vd));
    double score = (w / vd) + explore_factor * explore_score;
    if ( debug_p && explore_factor == 0.0 ) {
      printf("  ia(%d) w(%f) vd(%f) es(%f) sc(%f)\n",
             NODE[c].ia, w, vd, explore_score, score); }
    // XXX Update position in queue
    // theta_reinsert( c );
    if ( score > best_score ) {
      best_score = score;
      best_child = c; } }

  if ( debug_p && explore_factor == 0.0 ) {
    printf("Choosing %d\n", NODE[best_child].ia); }
  if ( best_child == NULL_NODE ) {
    fprintf(stderr, "No child!\n");
    exit(1); }

  return best_child; }

action decide( node_ptr gt, state st ) {
  uint32_t iters = 0;
  double deadline = current_ms() + 1000;
  do {
    iters++;
    state sti = st;
    node_ptr gti = gt;

    if ( debug_p ) {
      printf("starting iteration %d\n", iters); }
    
    // Tree Policy
    while ( NODE[gti].na == 0 && NODE[gti].lc != NULL_NODE ) {
      if ( debug_p ) {
        printf("tree_policy %d\n", gti); }
      gti = select(gti, 1.0);
      sti = eval(sti, NODE[gti].ia); }

    // Expand
    if ( ! (NODE[gti].na == 0) &&
         // Stunting: Don't expand when no free space
         ( ! STUNTING || ! (free_ptr == NULL_NODE)) ) {
      if ( debug_p ) {
        printf("expand %d\n", gti); }
      action m = NODE[gti].na - 1;
      NODE[gti].na = next_legal( sti, m );
      actor lastp = who(sti);
      sti = eval(sti, m);
      gti = alloc_node( gti, lastp, m, sti ); }

    // Default Policy
    while ( ! terminal_p(sti) ) {
      action how_many_actions = estimate_legal( sti );
      action a;
      do { a = rand() % how_many_actions; }
      while ( ! legal_p( sti, a ) );
      if ( debug_p ) {
        render_st(sti); }
      sti = eval(sti, a); }
    
    // Back propagate
    actor w = winner(sti);
    while ( gti != NULL_NODE ) {
      NODE[gti].w +=
        NODE[gti].wh == w ? 1.0 :
        (w == 0 ? 0.5 : 0.0);
      NODE[gti].v++;
      gti = NODE[gti].pr; }
  } while ( current_ms() < deadline );

  printf("Took %d steps, %d nodes total\n", iters, node_count);
  node_ptr bc = select(gt, 0.0);
  action a = NODE[bc].ia;
  return a; }

node_ptr choose( node_ptr gt, action a ) {
  if ( gt == NULL_NODE ) { return gt; }

  node_ptr r = NULL_NODE;
  node_ptr c = NODE[gt].lc;
  NODE[gt].lc = NULL_NODE;
  free_node(gt);
  while ( c != NULL_NODE ) {
    node_ptr t = c;
    c = NODE[t].rs;
    NODE[t].rs = NULL_NODE;
    NODE[t].pr = NULL_NODE;
    if ( NODE[t].ia == a ) {
      r = t; }
    else {
      free_node_rec(t); } }

  printf("Now %d nodes total\n", node_count);

  return r; }

void play() {
  state st = st0;
  node_ptr gt = NULL_NODE;
  actor lastp = 0;

  while (1) {
    if ( gt == NULL_NODE ) {
      gt = alloc_node( NULL_NODE, lastp, 0, st ); }
    printf("Expected value is %f\n",
           (NODE[gt].w / ((double)NODE[gt].v)));

    render_st(st);
    if ( terminal_p(st) ) {
      break; }
    
    actor thisp = who(st);
    action a;
    if ( thisp == 1 ) {
      char c = ' ';
      do {
        if ( ! (c == '\n') ) {
          printf("> "); }
        if ( scanf("%c", &c) == EOF ) {
          exit(1); }
      } while ( ! (decode_action(st, c, &a) && legal_p(st, a)) ); }
    else {
      a = decide(gt, st); }
    
    st = eval(st, a);
    gt = choose(gt, a);
    lastp = thisp; }

  actor w = winner(st);
  if ( w == 0 ) {
    printf("Draw\n"); }
  else {
    printf("Winner is %s\n", w == 1 ? "Player" : "Computer"); }
  return; }

int main() {
  srand(time(NULL));
  printf("State is %4lu  B\n", sizeof(state));
  printf("Node  is %4lu  B\n", sizeof(node));
  printf("Pool  is %4lu KB\n", ((sizeof(NODE))/1024));
  initialize_pool();
  play();
  return 0; }
