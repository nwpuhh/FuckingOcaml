open Graph

(*
* define the function of examing that is ending or not
* If the end id is in the outedges of the vertex, so it ends; no that ends not
*)
let rec is_ending outedges to_id =
  match outedges with
    | [] -> false
    | (label, id) :: rest -> if id = to_id then true else to_in_outedges rest to_id

    
(*
* define the function of judging the vertex has a proceding vertex or not
*)    
let no_next vertex_info = 
  if List.length vertex_info.outedges = 0 then true else false


(*
* define the function of getting the label by giving the id from the edges
*)
let get_label_by_id edges id_t = 
  let (label, i) = List.find (fun (l, id) -> id = id_t) edges in
  label

(*
* define the function of is_visited in the stack of the vertexs visited
*)
let rec is_visited vertexs_visited id =
  let vertex_copy = Stack.copy vertexs_visited in (*by using the copy function so that the stack do not be modified*)
    if (not (Stack.is_empty vertex_copy))
    then 
      let id_top = Stack.pop vertex_copy in
        if id_top = id then true
        else
        is_visited vertex_copy id
    else
      false
  
(*
* define the function of judging the point is the last point for the ouedges
*)
let is_last_next_point outedges id =
  let length = List.length outedges -1 in
  let (label, id_l) = List.nth outedges length in
  if id_l = id then true else false

(*
* defien the function of getting the next point id, 
* which is predefine the id is not the last one
*)
let get_next_point id_father id_son = 
  

let dfs graph from_id to_id =
  (*create a stack to store the id of vertexs which are visited before, and store the order*)
  let vertexs_visited = Stack.create() in

  (*define the function inside, where the vertexs_visited is the stack of storing the vertexs
  * path_acu is the list of edges which are listed in the path
  *)
  let rec get_path graph from_id to_id vertexs_visited path_acu = 

    let vertex_from_info = Graph.find_vertex graph from_id in
    (*If there is a direct path between the from_id and the to_id, which means that we can finish the path finding*)
    if(is_ending vertex_from_info.outedges to_id)
    then 
      Stack.push from_id vertexs_visited ;
      path_acu :: (from_id get_label_by_id vertex_from_info.outedges to_id) (*finish the finding of the path*)
    (*If there is no direct path between the from_id and to_id*)
    else
      (*First, we check that is there any outedges in the from_vertex*)
      if(no_next vertex_from_info)
      then 

        let id_prece = Stack.top vertexs_visited in (*get the father point*)
        let vertex_prece_info = Graph.find_vertex id_prece in (*get the father point info*)

        (*If this point is the last next point for the father point*)
        if (is_last_next_point vertex_prece_info.outedges from_id)
        then
          (*pop the father point and get the path from point grand-father*)
          let id_prece_ = Stack.pop vertexs_visited in
          get_path




          if no_next vertex_from_outedges 
          (*If it does not have next point*)
          then

