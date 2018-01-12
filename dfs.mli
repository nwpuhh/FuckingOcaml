open Graph
(*define the interface of dfs*)
(*
  * here we predefine the edge of the graph is the type of int
  * because the edges in the graph of getting the max flow are int
  * So we use the list of (id * int * id) list to indicate the path which is found between in the source and the sink
  * by using the method of depth-first search
  *)
val dfs: ('v, int) graph -> id -> id -> (id * int * id) list