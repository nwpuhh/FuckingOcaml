open Graph

(*
 * bfs:
 * Applies a Breadth First Search algorithm on a given graph (explores all
 * vertices) and returns the first path found between a given source and
 * destination vertex ids.
 *)
val bfs: ('v, int) graph -> id -> id -> (id * int * id) list
