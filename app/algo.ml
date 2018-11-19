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
