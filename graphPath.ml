(*relaise the function defined in the graphPath.mli *)
type ('v, 'e) graphPath

type id = string

type ('v, 'e) vertex_info_path = 
{
	label: 'v;
	id: id;
	outedge: ('e*id);
	inedge: ('e*id)
}

type ('v,'e) graphPath = 
{ (*Hashtable mapping identifiers to vertext_info_path*)
  vertices_path: (id, ('v,'e)vertex_info_path ) Hashtbl.t ;
}

(*---------------Constructor-------------------*)
let new_graphPath () = { vertices_path = Hashtbl.create 60 }

let add_vertex graphPath label id = 
  






