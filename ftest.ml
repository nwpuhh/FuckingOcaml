open Graph
open Fordfulkerson

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
  let max_flow = Fordfulkerson.max_flow graph2 _source _sink in
  Printf.printf "Max flow from %s to %s: %d\n" _source _sink max_flow;

  (* Rewrite the graph that has been read. *)
  let () = Gfile.export outfile graph in
  ()
