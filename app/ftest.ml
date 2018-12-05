open Graph
open Algo

let () =

  if Array.length Sys.argv <> 5 then
    begin
      Printf.printf "\nUsage: %s infile source sink outfile\n\n%!" Sys.argv.(0) ;
      exit 0
    end ;

  let infile = Sys.argv.(1)
  and outfile = Sys.argv.(4)

  (* These command-line arguments are not used for the moment. *)
  and _source = Sys.argv.(2)
  and _sink = Sys.argv.(3)
  in

  (* Open file *)
  let graph = Gfile.from_file infile in
  (* export .dot *)
  let graph = Algo.make_flow graph in
  let acu = Algo.trouver_chemin graph "0" "5" in
  let () =	Printf.printf "Val : %i\n" acu in
  let graph = Algo.convert_flow graph in 
  let () = Gfile.export "testexport" graph in
  (* Rewrite the graph that has been read. *)
  let () = Gfile.write_file outfile graph in

  ()
