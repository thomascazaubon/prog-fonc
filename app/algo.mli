open Graph

(***************  FORD FULKERSON *********************)

(* Iterate on all nodes.
 * v_iter gr f
 * f is applied with each node: f id (list-of-successors) *)
val make_flow: string graph -> (int * int) graph

val convert_flow: (int * int) graph -> string graph
