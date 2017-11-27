open Graph

(*
 * Reconstructs a path (list of (source, capacity, dest)) leading to a given
 * destination vertex using a "backtracking" hashtable documented in "bfs" later
 * in this file.
 *)
let rec path_from_backtrack graph dest backtrack =
    try
        let prev_vert_id = Hashtbl.find backtrack dest in
        let prev_vert = Graph.find_vertex graph prev_vert_id in
        let (capacity, destination) = List.find (fun (_, destination) -> String.equal destination dest) prev_vert.outedges in
        (path_from_backtrack graph prev_vert_id backtrack) @ [(prev_vert_id, capacity, destination)]
    with
        Not_found -> []

(*
 * bfs_aux:
 * Usual BFS recursion. Expects an empty queue on first call, fills this queue
 * internally.
 *)
let rec bfs_aux graph vertex_id visited_vertices queue = 
    let vertex = find_vertex graph vertex_id in

    List.iter (fun (label, next_id) ->
              if (not (Hashtbl.mem visited_vertices next_id) && label > 0) then
                  (Hashtbl.add visited_vertices next_id vertex_id ;
                   Queue.push next_id queue);
              ())
              vertex.outedges ;

    try bfs_aux graph (Queue.pop queue) visited_vertices queue
    with Queue.Empty -> () 

(* Documented in bfs.mli *)
let bfs graph source sink = 
    (*
     * visited_vertices is a handy structure that serves two uses in our
     * algorithm.
     * 
     * As its name suggests, it is used to store (in the keys part of the
     * hashtable) the visited vertices. This ensures our BFS does not loop.
     *
     * However, when a vertex V is visited, our implementation also stores in
     * the value part of the hashtable the vertex that led it to V. This can be
     * used later on by path_from_backtrack to reconstruct a path that leads to
     * a given vertex. Eventually, this provides a list of edges between source
     * and sink.
     *)
    let visited_vertices = Hashtbl.create 60 in
    let queue = Queue.create () in

    bfs_aux graph source visited_vertices queue;

    path_from_backtrack graph sink visited_vertices
