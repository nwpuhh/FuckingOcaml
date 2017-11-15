(* val dfs: ('v1, 'e1) graph -> id -> (('v, 'e) vertex_info -> unit) -> unit *)
let dfs graph vertex exec = 
    exec vertex ;
    let callNextDfs next_label next_id = dfs graph next exec in
    List.iter callNextDfs source.outedges  (* TODO: it actually iterates on edges!*)
