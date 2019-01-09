(* Read a graph from a file,
 * Write a graph to a file. *)

open Graph

type path = string

(*
(* To convert a file from the FF demo website to a readable file for from_file *)
val convert_file: path -> path
*)

(* Values are read as strings. *)
val from_file: path -> string graph

(* Similarly, we write only a string graph.
 * Use Graph.map if necessary to prepare the input graph. *)
val write_file: path -> string graph -> unit

(* Export en .dot *)
val export: path -> string graph -> unit
