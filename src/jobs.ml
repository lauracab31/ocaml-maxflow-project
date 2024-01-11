open Graph
open Tools

type matricule = {
  entite : string;
  id : int;
}

type person = {
  name : string;
  jobs_list : string list;
}

let jobs_to_record fichier =

  let infile = open_in fichier in
  
  let rec loop acc=
    try
      let line = input_line infile in

      let tab = String.split_on_char ' ' line in

      let job_list =

        match tab with
          | [] -> failwith "Fichier innaproprié"
          | s::rest -> {name = s ; jobs_list = rest}::acc
      in      
      loop job_list

    with End_of_file -> acc
  in

  let res = loop [] in

  close_in infile;
  res
;;

let record_to_graph list =

  let graph = empty_graph in 

  let graphs = new_node graph 0 in 

  let grapht = new_node graphs 1 in 

  let correspo = [] in

  let add_person g p c =

    let id = (nb_nodes g 2) in

    let graph = new_node g id in 

    let graphe = new_arc graph {src=0 ; tgt=id ; lbl="1"} in

    let nc = {entite=p.name ; id=id}::c in 

    let rec add_jobs g j c idp =

      match j with
        | [] -> (g,c)
        | x::rest -> begin 
                    try 
                      let x = List.find (fun a -> a.entite=x) c in 
                      add_jobs (new_arc g {src=idp;tgt=x.id;lbl="1"}) rest c idp


                    with Not_found -> let nouv = {entite=x;id=(nb_nodes g 2)} in 
                                    add_jobs (new_arc(new_arc (new_node g nouv.id) {src=nouv.id;tgt=1;lbl="1"}) {src=idp;tgt=nouv.id;lbl="1"}) rest (nouv::c) idp
                    
                    
        end
    in  

      add_jobs graphe p.jobs_list nc id

  in

  let rec parcours_list g c = function
    | [] -> (g,c)
    | x::rest -> let next = add_person g x c in 
                  parcours_list (fst next) (snd next) rest

    in 

  parcours_list grapht correspo list

;;



