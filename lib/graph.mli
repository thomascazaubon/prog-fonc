
(* Type of a directed graph in which arcs have labels of type 'a. *)
type 'a graph

(* Each node has a unique identifier (a name). *)
type id = string

exception Graph_error of string


(**************  CONSTRUCTORS  **************)

(* The empty graph. *)
val empty_graph: 'a graph

(* Add a new node with the given identifier.
 * @raise Graph_error if the id already exists. *)
val add_node: 'a graph -> id -> 'a graph

(* add_arc gr id1 id2 lbl  : adds an arc from node id1 to node id2 with label lbl
 * If an arc already exists between id1 and id2, its label is replaced by lbl. 
 * @raise Graph_error if id1 or id2 does not exist in the graph. *)
val add_arc: 'a graph -> id -> id -> 'a -> 'a graph


(**************  GETTERS  *****************)

(* node_exists gr id  indicates if the node with identifier id exists in graph gr. *)
val node_exists: 'a graph -> id -> bool

(* Type of lists of outgoing arcs of a node. 
 * An arc is represented by a pair of the destination identifier and the arc label. *)
type 'a out_arcs = (id * 'a) list

(* Find the out_arcs of a node.
 * @raise Graph_error if the id is unknown in the graph. *)
val out_arcs: 'a graph -> id -> 'a out_arcs

(* find_arc gr id1 id2  finds an arc between id1 and id2 and returns its label. Returns None if the arc does not exist. 
* @raise Graph_error if id1 is unknown. *)
val find_arc: 'a graph -> id -> id -> 'a option


(**************  COMBINATORS, ITERATORS  **************)

(* Iterate on all nodes.
 * v_iter gr f 
 * f is applied with each node: f id (list-of-successors) *)
val v_iter: 'a graph -> (id -> 'a out_arcs -> unit) -> unit

val v_fold: 'a graph -> ('b -> id -> 'a out_arcs -> 'b) -> 'b -> 'b

(* maps all arcs of the graph
 * Nodes keep the same identifiers. *)
val map: 'a graph -> ('a -> 'b) -> 'b graph


