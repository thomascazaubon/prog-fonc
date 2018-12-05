(* Convertit un graphe quelconque en graphe de flots *)
let make_flow gr =
  Graph.map gr (fun a -> (0, int_of_string a))

(* Convertit un graphe de flot en string graph affichable par ftest *)
let convert_flow gr =
  Graph.map gr (fun (a,b) -> (string_of_int a)^"/"^(string_of_int b))


exception Chemin_Not_Found
let trouver_chemin graphe src dest =
	let rec find_g graphe src dest acu = 
		if src = dest then acu
		else
			let arc_infos = Graph.out_arcs graphe src in
				let rec chemin arc_infos = match arc_infos with
					|[] -> raise Chemin_Not_Found
					|(i, (a,b)) :: rest -> 
						if (a<b) then 
							let () = Printf.printf "Val passage : %s\n%!" i in
							find_g graphe i dest (if (b-a)<acu then (b-a) else  acu) 
						else 
							 chemin rest
				in chemin arc_infos 
	in find_g graphe src dest 100
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

