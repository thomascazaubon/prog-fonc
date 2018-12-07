open Graph

type 'a path = (id * 'a out_arcs)

(* Convertit un graphe quelconque en graphe de flots *)
let make_flow gr =
  Graph.map gr (fun a -> (0, int_of_string a))

(* Convertit un graphe de flot en string graph affichable par ftest *)
let convert_flow gr =
  Graph.map gr (fun (a,b) -> (string_of_int a)^"/"^(string_of_int b))

(* 2 -> 1 -> 3 -> 5 ("2", [("1", (4,4)); ("3", (14,14)); ("5",(20,20))]) *)

let rec existe idx = function
  |[] -> false
  |(x,_)::tail -> if (x = idx) then true else existe idx tail

exception Chemin_Not_Found
let trouver_chemin graphe src dest =
	let acu = (src, []) in
	let rec find_g graphe src dest acu notpass =
		if src = dest then
			match acu with
				|(src, lst) -> (src, List.rev lst)
		else
			let arc_infos = Graph.out_arcs graphe src in
				let rec chemin arc_infos = match arc_infos with
					|[] -> begin
							match acu with
								|(deb, []) -> raise Chemin_Not_Found
                |(deb, (s,_)::[]) -> find_g graphe deb dest (deb, []) (src::notpass)
								|(deb, (s,_)::(s2,l)::lst) -> let () = Printf.printf "Passage supp : %s source : %s \n%!" s2 src in  find_g graphe s2 dest (deb, (s2,l)::lst) (src::notpass)
						   end
					|(i, (a,b)) :: rest ->
						if (a<b && not(List.mem i notpass)) then

                match acu with
  								|(first, lst) ->
                    if (not(existe i lst)) then
                       let () = Printf.printf "Passage a : %s\n%!" i in find_g graphe i dest (first, (i, (a,b))::lst) notpass
                    else
                      chemin rest

						else
							 chemin rest
				in chemin arc_infos
	in find_g graphe src dest acu []



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
  let rec iter max_flow = function
    |(_,[]) -> max_flow
    |(o,(s,(f,c))::tail) -> if ((c - f) < max_flow) then iter (c - f) (s,tail) else iter max_flow (o,tail)
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
    |(s,(ids,(f,c))::_) -> iter (Graph.add_arc gr s ids (f,c)) path

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
        let path = trouver_chemin output src dest in
        let max_flow = find_max path in
        let increased_path = increase_path path max_flow in
        let output = update_path output increased_path in
        iter output src dest
      end
    with
      |Chemin_Not_Found -> output
  in
  iter graph src dest
