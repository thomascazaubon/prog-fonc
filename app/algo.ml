let make_flow gr =
  Graph.map gr (fun a -> (0, int_of_string a))

let convert_flow gr =
  Graph.map gr (fun (a,b) -> (string_of_int a)^"/"^(string_of_int b))
