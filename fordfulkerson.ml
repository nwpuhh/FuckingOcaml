open Bfs
open Dfs

(*
 * augment_flow:
 * Modifies a given graph to reduce the capacity of each edge in a given path
 * by a given bottleneck value.
 *)
let augment_flow graph bottleneck path =
    let modify_edge (prev, capacity, next) =
        Graph.add_edge graph prev next (capacity - bottleneck) in
    List.iter modify_edge path

(*
 * min_capacity:
 * Given a path (list of (source, capacity, dest)) returns the max flow that can
 * flow through this path. (i.e: the minimum capacity of each edges)
 *)
let rec min_capacity = function
  | [] -> 10000000
  | (_, capa, _) :: tail -> min capa (min_capacity tail)

(* Documented in fordfulkerson.mli *)
let rec max_flow graph source sink =
    (* As long as the graph has an augmenting path *)
    let augmenting_path = Bfs.bfs graph source sink in
    if List.length augmenting_path > 0 then

        (* Augment the flow along this path with the bottleneck capacity *)
        let bottleneck = min_capacity augmenting_path in
        augment_flow graph bottleneck augmenting_path;

        (*
         * The max flow of a graph G with an augmenting path P is the sum of the
         * bottleneck capacity of P with the max_flow of G with P augmented by
         * this same bottleneck capacity.
         *)
        bottleneck + max_flow graph source sink
    else
        0


let rec max_flow_dfs graph source sink =
    (* As long as the graph has an augmenting path *)
    let augmenting_path = Dfs.dfs graph source sink in
    if List.length augmenting_path > 0 then

        (* Augment the flow along this path with the bottleneck capacity *)
        let bottleneck = min_capacity augmenting_path in
        augment_flow graph bottleneck augmenting_path;

        (*
         * The max flow of a graph G with an augmenting path P is the sum of the
         * bottleneck capacity of P with the max_flow of G with P augmented by
         * this same bottleneck capacity.
         *)
        bottleneck + max_flow_dfs graph source sink
    else
        0
