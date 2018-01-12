(*-------------------not finished------------------*)
(*give the definition of the graphPath*)
(*which is a set of vertices have the labels of type 'v*)
(*and the edges have labels of type 'e*)
type ('v, 'e) graphPath

(*each vertec has a unique identifier*)
type id = string

(*Info about vertex*)
type ('v,'e) vertex_info_path = 
{
	(*each vertex has a label*)
	label : 'v;
	(*each vertex has a id*)
	id : id;
	(*special thing: for all the vertexs in a path has only one outedge*)	
	outedge: ('e * id);
	(*Same to the inedge*)
	inedge: ('e * id)
}

(*--------Constructor----------*)
(*define a new graphPath *)
val new_graphPath: unit-> ('v, 'e) graphPath

(*add vertex in the graphPath*)
val add_vertex: ('v, 'e) graphPath -> 'v -> id -> unit

(*add_edge given the ids of origin and the dest*)
val add_edge: ('v, 'e) graphPath -> id1 -> id2 -> 'e -> unit

val remove_vertex: ('v, 'e) graphPath -> id -> unit

val remove_edge: ('v, 'e) graphPath -> id1 -> id2 -> unit

