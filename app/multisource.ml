open Graph
open Algo
(* Used to generate a max flow graph with multi source and multi sink *)
let () =

  let infile = Sys.argv.(1)
  and outfile = Sys.argv.(4)
  and nbsource = int_of_string Sys.argv.(2)
  and nbsink = int_of_string Sys.argv.(3)
  and lsource = []
  in
  (* Retrieve the sources and sink given in argument in order to return a tuple (list of sources, list of sink)  *)
  let multi nbsource nbsink lsource =
	let rec multi_rec nbsource nbsink lsource lsink i =
		if (nbsource<1) then
			if (nbsink<1) then (lsource,lsink)
			else multi_rec nbsource (nbsink-1) lsource (Sys.argv.(i)::lsink) (i+1)
		else multi_rec (nbsource-1) nbsink (Sys.argv.(i)::lsource) lsink (i+1)
	in multi_rec nbsource nbsink lsource [] 5
  in let lsourcedest = multi nbsource nbsink lsource in

  (* Open file *)
  let graph = Gfile.from_file infile in
  (* Add virtual source and virtual sink *)
  let graph = Algo.make_multi graph lsourcedest in
  (* export .dot *)
  let graph = Algo.make_flow graph in

  let graph = Algo.ford_fulkerson graph "VSRC" "VDEST" in
  let graph = Algo.convert_flow graph in
  let () = Gfile.export "FF_export" graph in
  (* Rewrite the graph that has been read. *)
  let () = Gfile.write_file outfile graph in

  ()
