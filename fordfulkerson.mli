open Graph

(*
 * max_flow:
 * Given a graph, a source and a sink, returns the maximum flow that can go
 * from source to sink.
 *)
val max_flow: ('v, int) graph -> id -> id -> int
