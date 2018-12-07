open Graph

type 'a path = (id * 'a out_arcs)

(* Convertit un graphe quelconque en graphe de flots *)
let make_flow gr =
  Graph.map gr (fun a -> (0, int_of_string a))

(* Convertit un graphe de flot en string graph affichable par ftest *)
let convert_flow gr =
  Graph.map gr (fun (a,b) -> (string_of_int a)^"/"^(string_of_int b))

(* 2 -> 1 -> 3 -> 5 ("2", [("1", (4,4)); ("3", (14,14)); ("5",(20,20))]) *)

exception Chemin_Not_Found
let trouver_chemin graphe src dest =
	let acu = (src, []) in
	let rec find_g graphe src dest acu =
		if src = dest then 
			match acu with 
				|(src, lst) -> (src, List.rev lst)
		else
			let arc_infos = Graph.out_arcs graphe src in
				let rec chemin arc_infos = match arc_infos with
					|[] -> raise Chemin_Not_Found
					|(i, (a,b)) :: rest ->
						if (a<b) then
							match acu with
								|(src, lst) -> find_g graphe i dest (src, (i, (a,b))::lst) 
							(*let () = Printf.printf "Val passage : %s\n%!" i in
							find_g graphe i dest (let )*)
						else
							 chemin rest
				in chemin arc_infos
	in find_g graphe src dest acu

let affichpath path = match path with
	|(i, lst) -> let () = Printf.printf "(%s, " i in
		let rec affich path = match path with 
		|(i, lst) -> match lst with
			|(a, (b,c)) :: rest -> let () =	Printf.printf "(%s, (%i,%i))" a b c in affich (i, rest)
			|[] -> Printf.printf ")\n" 
		in affich path
(*
let trouver_chemin graphe src dest = match graphe with
	|[]->raise Chemin_Not_Found
	|(id,lst) :: rest ->
		if id = src then
			let rec chemin (id,lst) acu = if id = dest then acu else
			 match lst with
				|[] -> raise Chemin_Not_Found
				|(i, (a,b)) :: rest ->
					if (a<b) then
						let arc_infos = Graph.out_arcs graphe i in
							chemin (i,arc_infos) (if (b-a)<acu then (b-a) else  acu)
					else
					  chemin (id,rest) acu

			in chemin (id,lst) 100
		else trouver_chemin rest src dest

*)

let find_max path =
  let rec iter max = function
    |[] -> max
    |(_,(f,c))::tail -> if ((c - f) < max) then iter (c - f) tail else iter max tail
  in
    iter max_int path

let update_path gr path =
  let rec iter acu path = match path with
    |(s,[]) -> acu
    |(s,(ids,(fs,cs))::(idd,(fd,cd))::tail) -> iter (Graph.add_arc acu ids idd (fd,cd)) (s,(idd,(fd,cd))::tail)
    |(s,[_]) -> acu
  in
  match path with
    |(_,[]) -> gr
    |(s,(ids,_)::_) -> let arc = Graph.find_arc gr s ids in match arc with
                                      |Some (f,c) -> iter (Graph.add_arc gr s ids (f,c)) path
                                      |_ -> assert false

let increase_path path value =
  let rec iter value acu path = match path with
    |(s,[]) -> (s, List.rev acu)
    |(s,(id,(f,c))::tail) -> iter value ((id,(f + value, c))::acu) (s,tail)
  in
  iter value [] path
