let is_visited vertexs_visited id_c =
  let vertex_copy = Stack.copy vertexs_visited in (*by using the copy function so that the stack do not be modified*)

  let rec is_visited_in_stack_copy vertex_stack_copy id = 
    if (not (Stack.is_empty vertex_stack_copy))
    then 
      let (id_top,_,_) = Stack.pop vertex_stack_copy in
        if id_top = id_c then true
        else
          is_visited_in_stack_copy vertex_copy id_c
    else
      false
  in
    is_visited_in_stack_copy vertex_copy id_c
;;
type id = string
;;
type ('v, 'e) vertex_info =
    { label: 'v ;
      id: id ;
      outedges: ('e * id) list ;
      inedges: ('e * id) list }
;;
type ('v, 'e) graph =
    { (* Hashtable mapping identifiers to vertex_info. *)
      vertices: (id, ('v, 'e) vertex_info) Hashtbl.t ;
    }    
;;
let find_vertex graph id =
  try Hashtbl.find graph.vertices id
  with Not_found -> failwith ("Vertex " ^ id ^ " cannot be found in this graph.")
;;
let find_edge graph id1 id2 =
  try
    let info = find_vertex graph id1 in
    let (e, _) = List.find (fun (_, id) -> id = id2) info.outedges in
      Some e
  with Not_found -> None;;

(**************  CONSTRUCTORS  **************)

let new_graph () = { vertices = Hashtbl.create 60 };;

let add_vertex graph label id =
  if Hashtbl.mem graph.vertices id then
    let info = Hashtbl.find graph.vertices id in
      Hashtbl.replace graph.vertices id { info with label }
  else
    Hashtbl.add graph.vertices id { label ; id ; outedges = [] ; inedges = [] };;

(* Insert or replace a pair (label * id) in a list. *)
let rec insert acu id label = function
  | [] -> (label, id) :: acu
  | (_, id2) as pair :: rest ->
      if id = id2 then List.rev_append rest ((label, id) :: acu)
      else insert (pair :: acu) id label rest;;

let add_edge graph id1 id2 elabel =

  (* Get vertex_info of origin and destination. *)
  let origin_info = find_vertex graph id1
  and dest_info   = find_vertex graph id2 in

  (* Add (or replace) the given edge. *)
  let new_outedges = insert [] id2 elabel origin_info.outedges
  and new_inedges  = insert [] id1 elabel dest_info.inedges in

    (* Put it back in the hashtable. *)
    Hashtbl.replace graph.vertices id1 { origin_info with outedges = new_outedges } ;
    Hashtbl.replace graph.vertices id2 { dest_info with inedges = new_inedges } ;

    (* Done. *)
    ();;


(**************  COMBINATORS, ITERATORS  **************)

let v_iter graph f = Hashtbl.iter (fun _ info -> f info) graph.vertices;;

(* Creates a new hashtable by mapping f to every value. *)
let map_hashtbl h f =
  let result = Hashtbl.create (Hashtbl.length h) in
    Hashtbl.iter (fun key v -> Hashtbl.add result key (f v)) h ;
    result
;;

let map graph vmap emap =
  let ret = new_graph () in
    v_iter graph (fun vi -> add_vertex ret (vmap vi.label) vi.id) ;
    v_iter graph (fun vi -> List.iter (fun (label, id2) -> add_edge ret vi.id id2 (emap label)) vi.outedges) ;
    ret
;;


(*
* define the function of getting the label by giving the id from the edges
*)
let get_label_by_id edges id_t = 
  let (label, i) = List.find (fun (l, id) -> id = id_t) edges in
    label
;;

(*
* define the function of is_visited in the stack of the vertexs visited
*)
let is_visited vertexs_visited id_c =
  let vertex_copy = Stack.copy vertexs_visited in (*by using the copy function so that the stack do not be modified*)

  let rec is_visited_in_stack_copy vertex_stack_copy id = 
    if (not (Stack.is_empty vertex_stack_copy))
    then 
      let (id_top,_,_) = Stack.pop vertex_stack_copy in
        if id_top = id_c then true
        else
          is_visited_in_stack_copy vertex_copy id_c
    else
      false
  in
    is_visited_in_stack_copy vertex_copy id_c
;;

(*
* define the function of getting path mode from the stack
* where the mode of elements in stack is (id, position, length), where position indicate the next element
* where the mode of path is (id, label, id) list, where label is the value between in two vertexs
*)
let get_path_from_stack graph vertex_stack =
  let rec loop graph stack path_acu = 
    let (id_top,_,_) = Stack.pop stack in
      if Stack.is_empty stack then path_acu
      else
        let (id_avant,_,_) = Stack.top stack in
        let vertex_info_avant = find_vertex graph id_avant in
        let label = get_label_by_id vertex_info_avant.outedges id_top in
        let path_new = List.append [(id_avant, label, id_top)] path_acu in
          loop graph stack path_new
  in
    loop graph vertex_stack []
;;
(*
* define the function of 
* there is no need to test the condition of no next vertex, beaucase if there is no next vertex, the length pushed in the stack will be 0
*)  
let rec get_path graph vertex_stack to_id =
  let (id_current, position, length) = Stack.top vertex_stack in
  let vertex_current_info = find_vertex graph id_current in

    (*if the top element has tried the last one next vertex, or is the vertex has visited*)
    if (position = length || is_visited vertex_stack id_current)
    then 
      (*The stack should pop the top one, and change the position of the father element*)
      let (_,_,_) = Stack.pop vertex_stack in (*pop the top*)
        (*Then judge it is the last element in the stack or not*)
        if Stack.is_empty vertex_stack then []
        (*if it is not the last one in the stack, so change the position of the father element*)
        else
          let (id_f, pos_f, len_f) = Stack.pop vertex_stack in
            Stack.push (id_f, pos_f+1, len_f) vertex_stack ;
            get_path graph vertex_stack to_id
    else
      (*Then we should consider the current vertex in the top of stack*)
      let outedges_current = vertex_current_info.outedges in
        (*if it is finished, so the last vertex is in the stack*)
        if id_current = to_id
        then 
          (*the function to change the stack to the path*)
          get_path_from_stack graph vertex_stack
          (*the normal condition, which means it can not find the end, but it has next id*)
        else 
          let (_,id_next) = List.nth outedges_current position in
          let vertex_next_info = find_vertex graph id_next in
          let outedges_next = vertex_next_info.outedges in
          let len_next = List.length outedges_next in
            Stack.push (id_next, 0, len_next) vertex_stack ;
            get_path graph vertex_stack to_id
;;

(*
* define the function of using dfs to find the path between the 
* from_id and to_id in the graph(where the label of edge should be int)
* the path will be returned in the type of (id*int*id) list
*)
let dfs graph from_id to_id =
(*
* define a stack to store the info of the vertex in the path
* The mode is (id, position, length) in the stack
* where id is the id of the vertex in the path;
* position and length indicate the place of the vertex, to help show it need pop the vertex or not
*)
  let vertex_info_in_path = Stack.create() in 

  let vertex_info_from = find_vertex graph from_id in
  let length_from = List.length vertex_info_from.outedges in
    Stack.push (from_id, 0, length_from);
    get_path graph vertex_info_in_path to_id
;;

1::[]



