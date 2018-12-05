open Graph

type 'a path = (id * 'a out_arcs)

(* Convertit un graphe quelconque en graphe de flots *)
let make_flow gr =
  Graph.map gr (fun a -> (0, int_of_string a))

(* Convertit un graphe de flot en string graph affichable par ftest *)
let convert_flow gr =
  Graph.map gr (fun (a,b) -> (string_of_int a)^"/"^(string_of_int b))

let find_max path =
  let rec iter max = function
    |[] -> max
    |(_,(f,c))::tail -> if ((c - f) < max) then iter (c - f) tail else iter max tail
  in
  iter max_int path
  ;;

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
