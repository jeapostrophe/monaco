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
#define POOL_SIZE UINT16_MAX
typedef uint16_t node_ptr;
#define NULL_NODE ((node_ptr)0)

const uint32_t SIMULATIONS_PER_ITERATION = 4;
const uint32_t MIN_ITERS = POOL_SIZE / SIMULATIONS_PER_ITERATION;

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
node_ptr recycled = 0;
node_ptr theta_head = NULL_NODE;

void theta_insert( node_ptr x ) {
  /* fprintf(stderr, "ti %d\n", x); */
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
  /* fprintf(stderr, "tr %d\n", x); */
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

void do_children( node_ptr pr, void (*f)(node_ptr) ) {
  node_ptr c = NODE[pr].lc;
  const char *lab = (f == theta_remove ? "remove" : "insert");
  /* fprintf(stderr, "< do %s on %d children\n", lab, pr); */
  while ( c != NULL_NODE ) {
    f(c);
    c = NODE[c].rs; }
  /* fprintf(stderr, "> do %s on %d children\n", lab, pr); */ }

void dg_edge(FILE *g, node_ptr x, node_ptr y, bool same) {
  if ( y != NULL_NODE ) {
    fprintf(g, "  n%d -> n%d\n", x, y);
    if ( same ) {
      fprintf(g, "  { rank = same; n%d; n%d; }\n", x, y); } } }
void dg_topoprint(FILE* g, node_ptr POS[], node_ptr target, node_ptr curr, node_ptr n) {
  if ( n == NULL_NODE ) { return; }

  fprintf(g, "  n%d [ shape = %s, label = \"%d @ %d\\nia(%d) na(%d)\\n%2.0f / %d\\n%2.2f\" ]\n",
          n, (n == target ? "house" : (n == curr ? "hexagon" : (NODE[n].wh == 1 ? "circle" : "square"))),
          n, POS[n], NODE[n].ia, NODE[n].na, 
          NODE[n].w, NODE[n].v, (100.0 * NODE[n].w / NODE[n].v));

  // XXX Is this possible without stack space?
  dg_topoprint(g, POS, target, curr, NODE[n].rs);
  dg_topoprint(g, POS, target, curr, NODE[n].lc); }

void dump_graph(node_ptr target, node_ptr curr) {
  FILE *g = fopen("graph.dot", "w");
  if ( ! g ) { perror("dump_graph"); exit(1); }

  fprintf(g, "strict digraph MCTS {\n");
  fprintf(g, "  rankdir = TB;\n");

  { node_ptr POS[POOL_SIZE] = {0};
    // Compute the index into the queue
    { node_ptr i = 1;
      node_ptr n = theta_head;
      do {
        POS[n] = i++;
        n = NODE[n].nq; }
      while ( n != theta_head ); }
    // Find the root node
    node_ptr root = curr;
    while ( NODE[root].pr != NULL_NODE ) {
      root = NODE[root].pr; }
    // Print topologically, from the root
    dg_topoprint(g, POS, target, curr, root);

    dg_topoprint(g, POS, target, curr, target); }

  fprintf(g, "  edge [ color = blue ]\n");
  for ( node_ptr n = 1; n < POOL_SIZE; n++ ) {
    dg_edge(g, n, NODE[n].pr, false); }

  fprintf(g, "  edge [ color = red ]\n");
  for ( node_ptr n = 1; n < POOL_SIZE; n++ ) {
    dg_edge(g, n, NODE[n].lc, false); }

  fprintf(g, "  edge [ color = green ]\n");
  for ( node_ptr n = 1; n < POOL_SIZE; n++ ) {
    dg_edge(g, n, NODE[n].rs, true); }

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

void free_node( node_ptr n ) {
  node_count--;

  assert( NODE[n].pq == NULL_NODE );
  assert( NODE[n].nq == NULL_NODE );
  assert( NODE[n].pr == NULL_NODE );
  assert( NODE[n].lc == NULL_NODE );
  assert( NODE[n].rs == NULL_NODE );

  NODE[n].rs = free_ptr;
  free_ptr = n;

  return; }

void free_node_rec( node_ptr n ) {
  if ( n == NULL_NODE ) { return; }
  free_node_rec(NODE[n].lc); NODE[n].lc = NULL_NODE;
  free_node_rec(NODE[n].rs); NODE[n].rs = NULL_NODE;
  NODE[n].pr = NULL_NODE;
  theta_remove(n);
  free_node(n); }

void recycle(node_ptr curr) {
  node_ptr last = NODE[theta_head].pq;
  if( last == NULL_NODE ) {
    fprintf(stderr, "recycle: no last\n");
    return; }
  node_ptr pr = NODE[last].pr;
  if( pr == NULL_NODE ) {
    fprintf(stderr, "recycle: last(%d) has no parent\n", last);
    return; }
  recycled++;
  node_ptr c = NODE[pr].lc;
  assert(c != NULL_NODE);
  NODE[pr].na = NODE[c].ia + 1;
  NODE[pr].lc = NODE[c].rs;
  NODE[c].rs = NULL_NODE;
  /* dump_graph(c, curr); */
  /* fprintf(stderr, "BEGIN killing %d\n", c); */
  free_node_rec(c);
  /* fprintf(stderr, "END killing %d\n", c); */ }

node_ptr alloc_node( node_ptr parent, actor lastp, action ia, state st ) {
  node_count++;
  node_ptr new = free_ptr;
  if ( new == NULL_NODE ) {
    recycle(parent);
    new = free_ptr;
    if ( new == NULL_NODE ) {
      fprintf(stderr, "alloc_node: Out of memory\n");
      fprintf(stderr, "tail is %d\n", NODE[theta_head].pq);
      dump_graph(NODE[theta_head].pq, parent);
      exit(1); } }
  
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
    // Remove the queue, so we don't kill this parent
    theta_remove( c );
    if ( score > best_score ) {
      best_score = score;
      best_child = c; } }

  if ( debug_p && explore_factor == 0.0 ) {
    printf("Choosing %d\n", NODE[best_child].ia); }
  if ( best_child == NULL_NODE ) {
    fprintf(stderr, "No child!\n");
    exit(1); }

  return best_child; }

action decide( uint32_t rounds, node_ptr gt, state st ) {
  recycled = 0;
  uint32_t iters = 0;
  uint32_t max_depth = 0;
  double start = current_ms();
  double deadline = start + 100;
  do {
    iters++;
    state sti = st;
    node_ptr gti = gt;
    uint32_t depth = 0;
    uint32_t wins[UINT8_MAX] = {0};
    theta_remove(gti);

    if ( debug_p ) {
      printf("starting iteration %d\n", iters); }
    
    // Tree Policy
    while ( NODE[gti].na == 0 && NODE[gti].lc != NULL_NODE ) {
      if ( debug_p ) {
        printf("tree_policy %d\n", gti); }
      gti = select(gti, 1.0);
      sti = eval(sti, NODE[gti].ia); depth++; }

    // Expand
    if ( ! (NODE[gti].na == 0) &&
         // XXX Detect if alloc_node will fail and have stunting turn on in that case
         // Stunting: Don't expand when no free space
         ( ! STUNTING || ! (free_ptr == NULL_NODE)) ) {
      if ( debug_p ) {
        printf("expand %d\n", gti); }
      action m = NODE[gti].na - 1;
      NODE[gti].na = next_legal( sti, m );
      actor lastp = who(sti);
      sti = eval(sti, m); depth++;
      gti = alloc_node( gti, lastp, m, sti );
      do_children(NODE[gti].pr, theta_remove); }

    if ( max_depth < depth ) {
      max_depth = depth; }

    // Default Policy
    state saved_sti = sti;
    for ( uint32_t simi = 0; simi < SIMULATIONS_PER_ITERATION; simi++ ) {
      sti = saved_sti;
      
      bool simulate_step1 = true;
      while ( ! terminal_p(sti) ) {
        action how_many_actions = estimate_legal( sti );
        action a;
        do { a = rand() % how_many_actions; }
        while ( ! legal_p( sti, a ) );
        if ( debug_p ) {
          render_st(sti); }

        // If stunting, then accrue statistics if we happen to choose an
        // existing child in the very first step.
        if ( simulate_step1 ) {
          node_ptr c = NODE[gti].lc;
          while ( c != NULL_NODE ) {
            if ( NODE[c].ia == a ) {
              gti = c; break; }
            c = NODE[c].rs; }
          simulate_step1 = false; }

        sti = eval(sti, a); }
    
      wins[winner(sti)]++; }
    
    // Back propagate
    while ( gti != NULL_NODE ) {
      NODE[gti].w += wins[NODE[gti].wh] + 0.5 * wins[0];
      NODE[gti].v++;
      gti = NODE[gti].pr;
      do_children(gti, theta_insert); }
  } while ( current_ms() < deadline || iters < MIN_ITERS );

  printf("Took %fms, %d steps, %d nodes total, recycled %d, max depth %d\n",
         (current_ms() - start), iters, node_count,
         recycled, max_depth);
  node_ptr bc = select(gt, 0.0);
  // dump_graph(bc, gt); exit(2);
  action a = NODE[bc].ia;
  return a; }

node_ptr choose( node_ptr gt, action a ) {
  if ( gt == NULL_NODE ) { return gt; }

  node_ptr r = NULL_NODE;
  node_ptr c = NODE[gt].lc;
  NODE[gt].lc = NULL_NODE;
  theta_remove(gt);
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

// XXX Be able to run experiments with differently configured AIs
// playing each other.
void play() {
  state st = st0;
  node_ptr gt = NULL_NODE;
  actor lastp = 0;
  uint32_t rounds = 0;

  while (1) {
    rounds++;
   
    if ( gt == NULL_NODE ) {
      gt = alloc_node( NULL_NODE, lastp, 0, st ); }
    printf("Expected value is %f\n",
           (NODE[gt].w / ((double)NODE[gt].v)));

    render_st(st);
    if ( terminal_p(st) ) {
      break; }
    
    actor thisp = who(st);
    action a;
    // XXX implement environment player
    // XXX implement a hidden information/move game to do MO-ISMCTS
    if ( false && thisp == 1 ) {
      char c = ' ';
      do {
        if ( ! (c == '\n') ) {
          printf("> "); }
        if ( scanf("%c", &c) == EOF ) {
          exit(1); }
      } while ( ! (decode_action(st, c, &a) && legal_p(st, a)) ); }
    else {
      a = decide(rounds, gt, st); }
    
    st = eval(st, a);
    gt = choose(gt, a);
    lastp = thisp; }

  actor w = winner(st);
  if ( w == 0 ) {
    printf("Draw\n"); }
  else {
    printf("Winner is %s\n", w == 1 ? "Player" : "Computer"); }
  printf("Rounds: %d\n", rounds);
  return; }

int main() {
  srand(time(NULL));
  printf("State is %4lu  B\n", sizeof(state));
  printf("Node  is %4lu  B\n", sizeof(node));
  printf("Pool  is %4lu KB\n", ((sizeof(NODE))/1024));
  initialize_pool();
  play();
  return 0; }
