open Graph

(****************** FLOW GRAPHS METHODS ********************)

(* A path is represented by an id representing its origin and a list of arcs leading to its destination *)
type 'a path = (id * 'a out_arcs)

(* Converts a string graph to a flow graph, ie the string label of each arc becomes the capacity and the flow is set to 0 as follows :
 * string:capacity -> (int:flow = 0 * int:capacity)
 *)
val make_flow: string graph -> (int * int) graph

(* Erase all residual arcs from a given graph *)
val erase_residual: (int * int) graph ->  (int * int) graph -> (int * int) graph

(* Used before exporting a flow graph, turns (int:flow * int:capacity) into "flow/capacity" *)
val convert_flow: (int * int) graph -> string graph

(* Make multi source/dest graph using a tuple containing the list ouf sources followed by the list of sinks *)
val make_multi: string graph -> (string list * string list) -> string graph

(* To erase the virtual nodes added by make_multi
 * CURRENTLY NOT WORKING (DOING NOTHING) !
 *)

val reverse_multi: (int * int) graph ->  (int * int) graph

(************ FORD FULKERSON SPECIFIC METHODS ***************)

(* Turns a flow graph into a residual graph *)
val make_residual: (int * int) graph -> (int * int) graph

(* Finds an increasable path in the given graph *)
val find_path: (int * int) graph -> Graph.id -> Graph.id -> (int * int) path

(* Displays a path (used for debug purposes) *)
val display_path: (int * int) path -> unit

(* Used to find the maximum possible flow increase on a path *)
val find_max: (int * int) path -> int

(* Increases the given path in the given graph with the given value *)
val update_path: (int * int) graph -> (int * int) path -> int -> (int * int) graph

(* Runs the ford fulkerson algorithm on the given graph from the specified source to the specified sink *)
val ford_fulkerson: (int * int) graph -> Graph.id -> Graph.id -> (int * int) graph
