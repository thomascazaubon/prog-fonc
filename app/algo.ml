open Graph

(* Exception used by the fid_path method to notify that there are no more possible paths in the graph *)
exception Path_Not_Found
(* The type of path, contains the id of the source and a list of arcs that lead to the sink *)
type 'a path = (id * 'a out_arcs)

let make_flow gr =
  Graph.map gr (fun a -> (0, int_of_string a))

(* This si used by make residual *)
let adaptative_add gr ids idd =
  (* Retrieving the orginal arcs that exist between the couple of nodes ids and idd *)
    let original_arc = Graph.find_arc gr ids idd in
    let residual_arc = Graph.find_arc gr idd ids in
    let arc_couple = (original_arc, residual_arc) in
    match arc_couple with
      (* If there was only one arc between both nodes,
       * then we just add the return arc which has the same capacity and a flow equal
       * to the flow of the capacity of the other minus its flow *)
      |(Some(fs,cs),None) -> Graph.add_arc gr idd ids ((cs-fs),cs)
      (* A second pattern that is equivalent to the first one, depends on the way the graph is explored *)
      |(None,Some(fs,cs)) -> Graph.add_arc gr ids idd ((cs-fs),cs)
      (* If there were already two arcs between the nodes, we need to sum both capacities and invert corresponding flows *)
      (* HERE IS A DANGEROUS CONDITION, WE NEED TO FIND A WAY TO AVOID MULTIPLE EXPLORATION OF THE SAME COUPLE OF NODES *)
      |(Some(fs,cs),Some(fd,cd)) when ((cs != cd) && ((fs + fd != cs) && (fs + fd != cd))) -> let gr = Graph.add_arc gr ids idd (cd,(cs+cd)) in
                                                                                              Graph.add_arc gr idd ids (cs,(cs+cd))
      (* These represent arcs that have already been treated by the previous pattern *)
      |(Some(fs,cs),Some(fd,cd)) -> gr
      (* Default, should never happen *)
      |(_,_) -> raise (Invalid_argument "Ho hell no !")

let rec make_residual_bis gr ids arcs = match arcs with
    |[] -> gr
    (* for each arc, a specific treatment is applied depending on ids and idd (done by adaptative_add) *)
    |(idd,_)::tail -> make_residual_bis (adaptative_add gr ids idd) ids tail

(* To make a residual graph, the function make_residual_bis is applied to the arcs of each nodes *)
let make_residual gr =
  v_fold gr (fun acu ids arcs -> make_residual_bis acu ids arcs) gr

(* This is used by erase_residual in order to reestablish the graph structure as it was before being turned to residual *)
let adaptative_erase original gr ids idd =
    (* Retrieve both pairs of arcs from the original graph and from the residual  *)
    let original_arc = Graph.find_arc original ids idd in
    let updated_arc = Graph.find_arc gr ids idd in
    let original_back_arc = Graph.find_arc original idd ids in
    let residual_arc = Graph.find_arc gr idd ids in
    let original_couple = (original_arc, original_back_arc) in
    let updated_couple = (updated_arc, residual_arc) in
    match original_couple with
      (* The two following patterns are matched when there was only one arc between the pair of nodes in the original graph
       * The flow in the original arc is simply replaced by the one that was calculated by Ford Fulkerson
       *)
      |(Some(fo,co), None) -> begin
                                match updated_couple with
                                |(Some(fu,_),_) -> Graph.add_arc original ids idd (fu,co)
                                |(_,_) -> raise (Invalid_argument "1")
                              end
      |(None, Some(fb,cb)) -> begin
                                 match updated_couple with
                                 (* The absolute value is used to counter the second passage ...*)
                                 |(_,Some(fr,cr)) -> Graph.add_arc original idd ids (fr,cb)
                                 |(_,_) -> raise (Invalid_argument "2")
                              end
      (* Here is the complicated case, if there was already two arcs between the pair of nodes,
       * their capacities have been summed in the residual graph, and their flows reversed, so we need to put back things as they were before while keeping the updated flow
       *)
      |(Some(fo,co), Some(fb,cb)) -> begin
                                       match updated_couple with
                                       (* The corrected_val variable is because the algorithm may treat twice the same problem,
                                        * since there are always 2 arcs between a pair of nodes in the residual graph,
                                        * so 1 of the two arcs would always be the opposite of its opponent without this condition.
                                        * First the original capacity of both arcs is restored by substracting the original capcity of the opposite arc_couple,
                                        * Then the flow is rescaled to fit in the original capacity by substracting the original capacity of the opposite arc.
                                        *)
                                       |(Some(fu,cu),Some(fr,cr)) -> let corrected_val = if ((fu -cb)<0) then 0 else fu-cb in let original = Graph.add_arc original ids idd (corrected_val,co) in
                                                                     let corrected_val = if ((fr -co)<0) then 0 else fr-co in
                                                                     Graph.add_arc original idd ids (corrected_val,cb)
                                       |(_,_) -> raise (Invalid_argument "3")
                                     end
      |(_,_) -> raise (Invalid_argument "4")

let rec erase_residual_bis original residual ids arcs = match arcs with
  |[] -> original
  |(idd,(f,c))::tail -> erase_residual_bis (adaptative_erase original residual ids idd) residual ids tail

(* To restore the original graph from its residual version, the function erase_residual_bis is applied to the arcs of each nodes *)
let rec erase_residual original residual =
  v_fold original (fun acu ids arcs -> erase_residual_bis acu residual ids arcs) original

(* Convert a flow graph to a format that allows exporting, ie (int * int) becomes string *)
let convert_flow gr =
  Graph.map gr (fun (a,b) -> (string_of_int a)^"/"^(string_of_int b))

(* A function to know whether a node has already been marked during the research of a path
 * We can't use List.mem because the value that we are looking for is a "complex" one
let rec exists idx = function
  |[] -> false
  |(x,_)::tail -> if (x = idx) then true else exists idx tail
 *)

(* A function that searches for a path in the given flow graph between the nodes src and dest *)
let find_path graphe src dest =
  (* path initialization *)
	let acu = (src, []) in
	let rec find_g graphe current dest acu marked =
    (* When current equals dest, the path is returned *)
		if current = dest then
			match acu with
				|(current, lst) -> (current, List.rev lst)
		else
      (* Fetches the out arcs of current *)
			let arc_infos = Graph.out_arcs graphe current in
      (* Marks current *)
      let marked = current::marked in
        (* To browse the arcs *)
				let rec chemin arc_infos = match arc_infos with
          (* if the current node is a well *)
					|[] -> begin
							match acu with
                (* If it is the source node, then there are no paths between src and dest *)
								|(deb, []) -> raise Path_Not_Found
                (* If the current node is not the source one, but is the first that is visited after src
                 * then we try another arc from src
                 *)
                |(deb, (s,_)::[]) -> find_g graphe deb dest (deb, []) marked
                (* If the current node is not the source one nor is the previous node, then we will try another arc from the previous one *)
								|(deb, (s,_)::(s2,l)::lst) -> find_g graphe s2 dest (deb, (s2,l)::lst) marked
						   end
          (* If there actually are some out arcs that are not saturated and go to a node that has not yet been marked *)
					|(i, (a,b)) :: rest -> if (a<b && not(List.mem i marked)) then
                                 begin
                                 match acu with
  								                |(first, lst) -> (*if (not(exists i lst)) then*) find_g graphe i dest (first, (i, (a,b))::lst) marked
                                 (*else chemin rest*)
                                 end
						                     else chemin rest
				in chemin arc_infos
	in find_g graphe src dest acu []

(* A function that displays a path (for debug purposes) *)
let display_path path = match path with
	|(i, lst) -> let () = Printf.printf "PATH := [%s -> " i in
		let rec display path = match path with
		|(i, lst) -> match lst with
			|(a, (b,c)) :: rest when (rest = []) -> let () =	Printf.printf "%s(%i,%i)" a b c in display (i, rest)
      |(a, (b,c)) :: rest -> let () =	Printf.printf "%s(%i,%i) -> " a b c in display (i, rest)
			|[] -> Printf.printf "]\n%!"
		in display path

(* A function that determines the maximum value a path can be increased by *)
let find_max path =
  let rec iter max_flow = function
    |(_,[]) -> max_flow
    |(o,(s,(f,c))::tail) -> if ((c - f) < max_flow) then iter (c - f) (s,tail) else iter max_flow (o,tail)
  in
    iter max_int path

(* A function that updates the given path in the given graph by the given value.
 * Note that when an arc is increased, its opposite is decreased by the same value
 *)
let update_path gr path value =
  let rec iter acu path = match path with
    |(s,[]) -> acu
    |(s,(ids,(fs,cs))::(idd,(fd,cd))::tail) -> begin
                                                  (* let () = Printf.printf "increasing...\n" in *)
                                                  let acu = (Graph.add_arc acu ids idd ((fd + value),cd)) in
                                                  match Graph.find_arc acu idd ids with
                                                  |Some(f2,c2) -> iter (Graph.add_arc acu idd ids ((f2 - value),c2)) (s,(idd,(fd,cd))::tail)
                                                  |None -> raise (Invalid_argument "Ho hell no !")
                                               end
    |(s,[_]) -> acu
  in
  match path with
    |(_,[]) -> gr
    |(s,(ids,(f1,c1))::_) -> let gr = Graph.add_arc gr s ids ((f1 + value),c1) in
                                    match Graph.find_arc gr ids s with
                                    |Some(f2,c2) -> iter (Graph.add_arc gr ids s ((f2 - value),c2)) path
                                    |None -> raise (Invalid_argument "Ho hell no !")

(* Executes the Ford Fulkerson algorithm on the given graph between src and dest.
 * Ends when the Path_Not_Found exception is risen by find_path
 *)
let ford_fulkerson graph src dest =
  let rec iter output src dest =
    try
      begin
        let path = find_path output src dest in
        let () = display_path path in
        let max_flow = find_max path in
        let () = Printf.printf "max = %d\n%!" max_flow in
        let output = update_path output path max_flow in
        iter output src dest
      end
    with
      |Path_Not_Found ->  let () = Printf.printf "[NO MORE PATHS]\n%!" in erase_residual graph output
  in
  iter (make_residual graph) src dest
