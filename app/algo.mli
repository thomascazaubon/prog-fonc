open Graph

(* Type of path
 * A path is represented by an id representing its origin and a list of arcs
 *)
type 'a path = (id * 'a out_arcs)

(***************  FORD FULKERSON *********************)

val make_flow: string graph -> (int * int) graph

val convert_flow: (int * int) graph -> string graph


val update_path: (int * int) graph -> (int * int) path -> (int * int) graph

val increase_path: (int * int) path -> int -> (int * int) path

val trouver_chemin: (int * int) graph -> Graph.id -> Graph.id -> (int * int) path

val affichpath: (int * int) path -> unit


