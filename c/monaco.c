action how_many_actions;
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

void initialize_pool() {
  node_ptr last = NULL_NODE;
  for ( node_ptr n = 1; n < POOL_SIZE; n++ ) {
    NODE[n].pq = last;
    NODE[n].nq = n+1;
    last = n; }
  NODE[last].nq = NULL_NODE;
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

node_ptr alloc_node( node_ptr parent, action ia, state st ) {
  node_count++;
  node_ptr new = free_ptr;
  if ( new == NULL_NODE ) {
    fprintf(stderr, "alloc_node: Out of memory\n");
    // XXX implement recycling
    exit(1); }
  
  free_ptr = NODE[new].nq;
  NODE[free_ptr].pq = NODE[new].pq;

  NODE[new].w = 0.0;
  NODE[new].v = 0;
  NODE[new].pr = parent;
  NODE[new].lc = NULL_NODE;
  NODE[new].rs = NULL_NODE;
  NODE[new].pq = NULL_NODE;
  NODE[new].nq = NULL_NODE;
  NODE[new].ia = ia;
  if ( terminal_p(st) ) {
    NODE[new].na = 0;
  } else {
    action a = next_legal( st, how_many_actions );
    NODE[new].na = a; }
  NODE[new].wh = who( st );

  if ( parent != NULL_NODE ) {
    NODE[new].rs = NODE[parent].lc;
    NODE[parent].lc = new; }
  
  return new; }

void free_node( node_ptr n ) {
  node_count--;
  node_ptr nq = NODE[n].nq;
  if ( nq != NULL_NODE ) {
    NODE[nq].pq = NODE[n].pq; }
  node_ptr pq = NODE[n].pq;
  if ( pq != NULL_NODE ) {
    NODE[pq].nq = nq; }

  if ( free_ptr != NULL_NODE ) {
    NODE[free_ptr].pq = n; }
  NODE[n].nq = free_ptr;
  NODE[n].pq = NULL_NODE;

  return; }

node_ptr select( node_ptr parent, float explore ) {
  double best_score = (-1.0/0.0);
  node_ptr best_child = NULL_NODE;

  double tlpv = log((double)NODE[parent].v);
  for ( node_ptr c = NODE[parent].lc;
        c != NULL_NODE;
        c = NODE[c].rs ) {
    double vd = ((double)NODE[c].v);
    double score =
      (NODE[c].w / vd)
      + explore * (sqrt(tlpv / vd));
    // XXX Update position in queue
    if ( score > best_score ) {
      best_score = score;
      best_child = c; } }

  if ( best_child == NULL_NODE ) {
    fprintf(stderr, "No child!\n");
    exit(1); }

  return best_child; }

action decide( node_ptr gt, state st ) {
  uint32_t iters = 0;
  double deadline = current_ms() + 100;
  do {
    iters++;
    state sti = st;
    node_ptr gti = gt;

    // Tree Policy
    while ( NODE[gti].na == 0 && NODE[gti].lc != NULL_NODE ) {
      gti = select(gti, 1.0);
      sti = eval(sti, NODE[gti].ia); }

    // Expand
    if ( ! (NODE[gti].na == 0) ) {
      action m = NODE[gti].na - 1;
      NODE[gti].na = next_legal( sti, m );
      sti = eval(sti, m);
      gti = alloc_node( gti, m, sti ); }

    // Default Policy
    while ( ! terminal_p(sti) ) {
      action a;
      do { a = rand() % how_many_actions; }
      while ( ! legal_p( sti, a ) );
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
    if ( gt == NULL_NODE ) {
      gt = alloc_node( NULL_NODE, 0, st ); }
    printf("Expected value is %f\n",
           (NODE[gt].w / ((double)NODE[gt].v)));
    
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
