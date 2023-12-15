open Graph

val clone_nodes: 'a graph -> 'b graph
val gmap: 'a graph -> ('a -> 'b) -> 'b graph
val add_arc: int graph -> id -> id -> int -> int graph
val find_path : 'a graph -> id -> id -> 'a arc list option
val print_arclist : 'a arc list option -> unit