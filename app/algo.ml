open Graph

exception Path_Not_Found
type 'a path = (id * 'a out_arcs)

let make_flow gr =
  Graph.map gr (fun a -> (0, int_of_string a))

let adaptative_add gr ids idd =
    let original_arc = Graph.find_arc gr ids idd in
    let residual_arc = Graph.find_arc gr idd ids in
    let arc_couple = (original_arc, residual_arc) in
    match arc_couple with
      |(Some(fs,cs),None) -> Graph.add_arc gr idd ids ((cs-fs),cs)
      (* HERE IS A DANGEROUS CONDITION THAT IS NOT SUFFICIENT AND THUS NEEDS TO BE IMPROVED *)
      |(Some(fs,cs),Some(fd,cd)) when ((cs != cd) && ((fs + fd != cs) && (fs + fd != cd))) -> let gr = Graph.add_arc gr ids idd (cd,(cs+cd)) in
                                                                                              Graph.add_arc gr idd ids (cs,(cs+cd))
      |(Some(fs,cs),Some(fd,cd)) -> gr
      |(None,_) -> raise (Invalid_argument "Ho hell no !")

let rec make_residual_bis gr ids arcs = match arcs with
    |[] -> gr
    |(idd,(f,c))::tail -> make_residual_bis (adaptative_add gr ids idd) ids tail

let make_residual gr =
  v_fold gr (fun acu ids arcs -> make_residual_bis acu ids arcs) gr

(***********************************************************************)
(***********************************************************************)
(*******************         TO BE IMPROVED          *******************)
(***********************************************************************)
(***********************************************************************)

let adaptative_erase original gr ids idd =
    let original_arc = Graph.find_arc original ids idd in
    let updated_arc = Graph.find_arc gr ids idd in
    let original_back_arc = Graph.find_arc original idd ids in
    let residual_arc = Graph.find_arc gr idd ids in
    let original_couple = (original_arc, original_back_arc) in
    let updated_couple = (updated_arc, residual_arc) in
    match original_couple with
      |(Some(fo,co), None) -> begin
                            (*  let () = Printf.printf "yolo\n%!" in *)
                              match updated_couple with
                              |(Some(fu,_),_) -> Graph.add_arc original ids idd (fu,co)
                              |(_,_) -> raise (Invalid_argument "1")
                              end
      |(Some(fo,co), Some(fb,cb)) -> begin
                                    (* let () = Printf.printf "swag\n%!" in*)
                                     match updated_couple with
                                     (* The corrected_val variable is because the algorithm may treat twice the same problem, so 1 arc would be the opposite of its opponent without this condition *)
                                     |(Some(fu,cu),Some(fr,cr)) -> let corrected_val = if ((fu -cb)<0) then 0 else fu-cb in let original = Graph.add_arc original ids idd (corrected_val,co) in
                                                                   let corrected_val = if ((fr -co)<0) then 0 else fr-co in
                                                                   Graph.add_arc original idd ids (corrected_val,cb)
                                     |(_,_) -> raise (Invalid_argument "2")
                                     end
      |(None, Some(fb,cb)) -> begin
                               let () = Printf.printf "swag\n%!" in
                               match updated_couple with
                               (* The absolute value is used to counter the second passage ...*)
                               |(_,Some(fr,cr)) -> Graph.add_arc original idd ids (fr,cb)
                               |(_,_) -> raise (Invalid_argument "3")
                               end
      |(_,_) -> (* let s = ids ^ " " ^ idd in raise (Invalid_argument s) *) (* let () = Printf.printf "done\n%!" in *) original

let rec erase_residual_bis original residual ids arcs = match arcs with
  |[] -> original
  |(idd,(f,c))::tail -> erase_residual_bis (adaptative_erase original residual ids idd) residual ids tail

let rec erase_residual original residual =
  v_fold original (fun acu ids arcs -> erase_residual_bis acu residual ids arcs) original

let convert_flow gr =
  Graph.map gr (fun (a,b) -> (string_of_int a)^"/"^(string_of_int b))

let rec exists idx = function
  |[] -> false
  |(x,_)::tail -> if (x = idx) then true else exists idx tail

let find_path graphe src dest =
	let acu = (src, []) in
	let rec find_g graphe src dest acu notpass =
		if src = dest then
			match acu with
				|(src, lst) -> (src, List.rev lst)
		else
			let arc_infos = Graph.out_arcs graphe src in
      let notpass = src::notpass in
				let rec chemin arc_infos = match arc_infos with
					|[] -> begin
							match acu with
								|(deb, []) -> raise Path_Not_Found
                |(deb, (s,_)::[]) -> (* let () = Printf.printf "%s Marqué !\n%!" src in*) find_g graphe deb dest (deb, []) (src::notpass)
								|(deb, (s,_)::(s2,l)::lst) -> (*let () = Printf.printf "Passage supp : %s source : %s \n%!" s2 src in *) find_g graphe s2 dest (deb, (s2,l)::lst) (src::notpass)
						   end
					|(i, (a,b)) :: rest ->
						if (a<b && not(List.mem i notpass)) then
                match acu with
  								|(first, lst) ->
                    if (not(exists i lst)) then
                       (* let () = Printf.printf "Passage a : %s\n%!" i in *) find_g graphe i dest (first, (i, (a,b))::lst) notpass
                    else
                      chemin rest
						else
							 chemin rest
				in chemin arc_infos
	in find_g graphe src dest acu []

let display_path path = match path with
	|(i, lst) -> let () = Printf.printf "PATH := [%s -> " i in
		let rec display path = match path with
		|(i, lst) -> match lst with
			|(a, (b,c)) :: rest when (rest = []) -> let () =	Printf.printf "%s(%i,%i)" a b c in display (i, rest)
      |(a, (b,c)) :: rest -> let () =	Printf.printf "%s(%i,%i) -> " a b c in display (i, rest)
			|[] -> Printf.printf "]\n%!"
		in display path

let find_max path =
  let rec iter max_flow = function
    |(_,[]) -> max_flow
    |(o,(s,(f,c))::tail) -> if ((c - f) < max_flow) then iter (c - f) (s,tail) else iter max_flow (o,tail)
  in
    iter max_int path

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

let increase_path path value =
  let rec iter value acu path = match path with
    |(s,[]) -> (s, List.rev acu)
    |(s,(id,(f,c))::tail) -> iter value ((id,(f + value, c))::acu) (s,tail)
  in
  iter value [] path

let ford_fulkerson graph src dest =
  let rec iter output src dest =
    try
      begin
        let path = find_path output src dest in
        let () = display_path path in
        let max_flow = find_max path in
        let () = Printf.printf "max = %d\n%!" max_flow in
        (*
        let increased_path = increase_path path max_flow in
        *)
        let output = update_path output path max_flow in
        iter output src dest
      end
    with
      |Path_Not_Found ->  erase_residual graph output
  in
  iter (make_residual graph) src dest

  (*
    let make_empty gr =
      let result = empty_graph in
      let f acu id out_arcs = add_node acu id in
      v_fold gr f result

    let rec make_residual_bis gr ids arcs = match arcs with
        |[] -> gr
        |(idd,(0,c))::tail -> make_residual_bis (Graph.add_arc gr ids idd (c, c)) ids tail
        |(idd,(f,c))::tail when f = c -> make_residual_bis (Graph.add_arc gr idd ids (c, c)) ids tail
        |(idd,(f,c))::tail -> make_residual_bis (Graph.add_arc (Graph.add_arc gr idd ids (f,c)) ids idd (c-f, c)) ids tail

    let make_residual gr =
      let rec iter residual_gr gr = match gr with
        |[] -> residual_gr
        |(ids,_)::tail -> iter (make_residual_bis residual_gr ids (Graph.out_arcs gr ids)) tail
      in
      iter (make_empty gr) gr
  *)
