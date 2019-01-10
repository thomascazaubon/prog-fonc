open Graph
open Printf

type path = string

(* Format of text files: lines of the form
 *
 *  v id               (node with the given identifier)
 *  e label id1 id2    (arc with the given (string) label. Goes from node id1 to node id2.)
 *
 *)

let write_file path graph =

  (* Open a write-file. *)
  let ff = open_out path in

  (* Write in this file. *)
  fprintf ff "=== Graph file ===\n\n" ;

  (* Write all nodes *)
  v_iter graph (fun id _ -> fprintf ff "v %s\n" id) ;
  fprintf ff "\n" ;

  (* Write all arcs *)
  v_iter graph (fun id out -> List.iter (fun (id2, lbl) -> fprintf ff "e \"%s\" %s %s\n" lbl id id2) out) ;

  fprintf ff "\n=== End of graph ===\n" ;

  close_out ff ;
  ()

let generate_graph number_of_nodes =
  let random_add gr ids maxi =
    (* the seed for random int generation is initialized *)
    let () = Random.self_init () in
    (* loops number_of_arcs times *)
    let rec loop gr ids maxi = function
      |0 -> gr
      |remaining_arcs -> begin
                          (* Randomly picks a destination, if it is 0 or maxi, the value needs to be corrected as it is not the actual id of the node in the graph !*)
                          let dest = string_of_int (Random.int (maxi+1)) in let dest = if (dest = "0") then "SRC" else if (dest = string_of_int maxi) then "DST" else dest in
                          (* Randomly picks the arc capacity, min is 1*)
                          let capacity = Random.int (maxi * 3) + 1 in
                          (* Checking that we are not adding a loop arc *)
                          if (not (dest = ids))
                          (* Adding the arc *)
                          then loop (Graph.add_arc gr ids dest (string_of_int capacity)) ids maxi (remaining_arcs-1)
                          (* Giving an other try *)
                          else loop gr ids maxi remaining_arcs
                         end
      in
      (* Choosing the number of arcs to be added for the current node *)
      (* If the current node is the source, we want to add more arcs, notice that there is always at least one arc created *)
      if (ids = "SRC") then loop gr ids maxi ((Random.int maxi)+1)
      else if (maxi > 5) then loop gr ids maxi ((Random.int (maxi/2))+1) else loop gr ids maxi ((Random.int maxi)+1)
  in
  let rec loop_arcs gr maxi = function
    |(-1) -> gr
    |current -> if (current != maxi) then let ids = if (current = 0) then "SRC" else string_of_int current in loop_arcs (random_add gr ids maxi) maxi (current-1) else loop_arcs gr maxi (current-1)
  in
  let rec loop_nodes maxi acu current =
    begin
      if (current = (maxi+1))
      then loop_arcs acu maxi maxi
      else let ids = if (current = 0) then "SRC" else if (current = maxi) then "DST" else string_of_int current in loop_nodes maxi (Graph.add_node acu ids) (current+1)
    end
  in
  loop_nodes (number_of_nodes-1) empty_graph 0

(* Reads a line with a node. *)
let read_node graph line =
  try Scanf.sscanf line "v %s" (fun id -> add_node graph id)
  with e ->
    Printf.printf "Cannot read node in line - %s:\n%s\n" (Printexc.to_string e) line ;
    failwith "from_file"

(* Reads a line with an arc. *)
let read_arc graph line =
  try Scanf.sscanf line "e \"%s@\" %s %s" (fun label id1 id2 -> add_arc graph id1 id2 label)
  with e ->
    Printf.printf "Cannot read arc in line - %s:\n%s\n" (Printexc.to_string e) line ;
    failwith "from_file"

let from_file path =

  let infile = open_in path in

  (* Read all lines until end of file. *)
  let rec loop graph =
    try
      let line = input_line infile in
      let graph2 =
        (* Ignore empty lines *)
        if line = "" then graph

        (* The first character of a line determines its content : v or e.
         * Lines not starting with v or e are ignored. *)
        else match line.[0] with
          | 'v' -> read_node graph line
          | 'e' -> read_arc graph line
          | _ -> graph
      in
      loop graph2
    with End_of_file -> graph
  in

  let final_graph = loop empty_graph in

  close_in infile ;
  final_graph

let export path graph =
	let out = open_out path in
		 	fprintf out "digraph finite_state_machine {\n";
			fprintf out "rankdir=LR;\n";
			fprintf out "size=\"8,5\"\n";
			fprintf out "node [shape = circle];\n";
			v_iter graph (fun id out2 -> List.iter (fun (id2, lbl) -> fprintf out "%s -> %s [ label = \"%s\" ];\n"  id id2 lbl) out2) ;
			fprintf out "}\n";
   close_out out ;
  ()
