(* Read a graph from a file,
 * Write a graph to a file. *)

open Graph

type path = string

(* To generate a random graph given the wanted number of nodes *)
val generate_graph: int -> string graph

(* Values are read as strings. *)
val from_file: path -> string graph

(* Similarly, we write only a string graph.
 * Use Graph.map if necessary to prepare the input graph. *)
val write_file: path -> string graph -> unit

(* Export en .dot *)
val export: path -> string graph -> unit
