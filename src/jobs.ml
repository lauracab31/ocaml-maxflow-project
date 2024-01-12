open Graph
open Tools
open Ford_fulkerson
open Printf

(* Type pour identifier le nom d'une personne ou d'un job à partir d'un id *)
type matricule = {
  entite : string;
  id : int;
}

(* Type qui contient une personne et la liste des offres reçues *)
type person = {
  name : string;
  jobs_list : string list;
}

(* Convertit un fichier texte contenant les personnes et leurs offres pour obtenir une liste de person *)
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

(* Génère à partir d'une liste de person, un tuple contenant un graphe d'association et une liste contenant les associations id-noms *)
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
                      let x = List.find (fun a -> a.entite=x) c in (* Applique l'algorithme de ford_fulkerson *)
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

(* Applique l'algorithme de Ford-Fulkerson au graphe et écrit dans un fichier les éventuelles affectations pour chaque personne *)
let output_file (graph,cor) fichier =

  let res = ff_ope graph 0 1 in 

  let affectation file e = if e.src > 1 && e.tgt > 1 && e.lbl = "1/1" then fprintf file "%s : %s \n\n" (List.find (fun i->i.id = e.src) cor).entite (List.find (fun i->i.id = e.tgt) cor).entite 
    
  else if e.src = 0 && e.lbl = "0/1" then fprintf file "%s : sans emploi \n\n" (List.find (fun i->i.id = e.tgt) cor).entite 
  
  in 

  let ff = open_out fichier in 

  e_iter res (affectation ff);

  close_out ff

;;