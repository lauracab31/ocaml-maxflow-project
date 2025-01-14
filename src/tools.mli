open Graph

val clone_nodes: 'a graph -> 'b graph
val gmap: 'a graph -> ('a -> 'b) -> 'b graph
val add_arc: int graph -> id -> id -> int -> int graph
val find_path : id graph -> id -> id -> id arc list option
val print_arclist : 'a arc list option -> unit
val nb_nodes : 'a graph -> int -> int