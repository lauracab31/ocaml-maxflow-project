open Graph

type matricule = {
  entite : string;
  id : int;
}

type person = {
  name : string;
  jobs_list : string list;
}

val jobs_to_record : string->person list
val record_to_graph : person list -> string graph * matricule list
val output_file : string graph * matricule list -> string -> unit