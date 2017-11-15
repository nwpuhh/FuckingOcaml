open Graph
open Dfs

let () =

  if Array.length Sys.argv <> 5 then
    begin
      Printf.printf "\nUsage: %s infile source sink outfile\n\n%!" Sys.argv.(0) ;
      exit 0
    end ;

  let infile = Sys.argv.(1)
  and _source = Sys.argv.(2)
  and _sink = Sys.argv.(3)
  and outfile = Sys.argv.(4) in

  let graph = Gfile.from_file infile in
  let graph2 = Graph.map graph (fun x -> x) int_of_string in
  let testDfs v = Printf.printf "DFS: %s\n" v.label in
  Dfs.dfs graph2 (find_vertex graph "source") testDfs;

  (* Rewrite the graph that has been read. *)
  let () = Gfile.export outfile graph in
  ()



