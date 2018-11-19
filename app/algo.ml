let make_flow gr =
  Graph.map gr (fun (a,b) -> (a, 0, b))
