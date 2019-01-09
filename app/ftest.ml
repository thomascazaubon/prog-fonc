open Graph
open Algo
(* 2 -> 1 -> 3 -> 5 ("2", [("1", (4,4)); ("3", (14,14)); ("5",(20,20))]) *)
let () =

  if Array.length Sys.argv <> 5 then
    begin
      Printf.printf "\nUsage: %s infile source sink outfile\n\n%!" Sys.argv.(0) ;
      exit 0
    end ;

  let infile = Sys.argv.(1)
  and outfile = Sys.argv.(4)

  and source = Sys.argv.(2)
  and sink = Sys.argv.(3)
  in

  (* Open file *)
  let graph = Gfile.from_file infile in
  (* export .dot *)
  let graph = Algo.make_flow graph in
(*  let graph = Algo.make_residual graph in *)
  let graph = Algo.ford_fulkerson graph source sink in
  let graph = Algo.convert_flow graph in
  let () = Gfile.export "FF_export" graph in
  (* Rewrite the graph that has been read. *)
  let () = Gfile.write_file outfile graph in

  ()
