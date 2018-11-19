open Graph

(***************  FORD FULKERSON *********************)

(* Iterate on all nodes.
 * v_iter gr f
 * f is applied with each node: f id (list-of-successors) *)
val make_flow: 'a graph -> 'b graph
