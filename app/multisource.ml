open Graph
open Algo
(* 2 -> 1 -> 3 -> 5 ("2", [("1", (4,4)); ("3", (14,14)); ("5",(20,20))]) *)
let () =

 (* if Array.length Sys.argv <> 5 then
    begin
      Printf.printf "\nUsage: %s infile source sink outfile\n\n%!" Sys.argv.(0) ;
      exit 0
    end ;*)

  let infile = Sys.argv.(1)
  and outfile = Sys.argv.(4)
  and source = Sys.argv.(2)
  and sink = Sys.argv.(3)
  and nbsource = int_of_string Sys.argv.(2)
  and nbsink = int_of_string Sys.argv.(3)
  and lsource = []
  in
  let multi nbsource nbsink lsource =
	let rec multi_rec nbsource nbsink lsource lsink i = 
		if (nbsource<1) then 
			if (nbsink<1) then (lsource,lsink)
			else multi_rec nbsource (nbsink-1) lsource (Sys.argv.(i)::lsink) (i+1)
		else multi_rec (nbsource-1) nbsink (Sys.argv.(i)::lsource) lsink (i+1) 
	in multi_rec nbsource nbsink lsource [] 5
  in let lsourcedest = multi nbsource nbsink lsource in

  (*let testlis lsourcedest =
	let rec test lsourcedest = match lsourcedest with
		|([],dest) -> (match dest with
			|[] -> Printf.printf "FINI\n%!"
			|i :: rest -> let () = Printf.printf "Dest : %s\n%!" i in test ([],rest))
		|(i :: rest,dest) -> let () = Printf.printf "Source : %s\n%!" i in test (rest,dest)
	in test lsourcedest
  in let () = testlis lsourcedest in*)

  (* Open file *)
  let graph = Gfile.from_file infile in
  let graph = Algo.make_multi graph lsourcedest in
  (* export .dot *)
  let graph = Algo.make_flow graph in
(*  let graph = Algo.make_residual graph in *)
  let graph = Algo.ford_fulkerson graph "source" "dest" in
  let graph = Algo.convert_flow graph in
  let () = Gfile.export "FF_export" graph in
  (* Rewrite the graph that has been read. *)
  let () = Gfile.write_file outfile graph in

  ()
