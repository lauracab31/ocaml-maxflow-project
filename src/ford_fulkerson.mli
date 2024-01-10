open Graph


type flot_node ={

  flot : int ;

  capacite : int }


val min_cap : id arc list option -> int

val flot_init : string graph -> flot_node graph

val ff_ope : string graph -> id -> id -> string graph