open Graph
open Tools

type flot_node ={

  flot : int ;

  capacite : int ;

}

(* Cherche la capacité minimum d'un chemin donné *)
let min_cap l =
  let rec min_capb l min = match l with
  |None | Some([]) -> min
  |Some (x::rest) -> if x.lbl < min then min_capb (Some rest) x.lbl else min_capb (Some rest) min
  
  in
  min_capb l 99999999
;;

(* Initialise un graphe de flots *)
let flot_init g = gmap g (fun x -> {flot = 0 ; capacite = int_of_string x});;

(* Initialise un graphe résiduel *)
let resi_init g = gmap g (fun x -> int_of_string x);;


(* Fait évoluer le flot en fonction de la capacité minimum *)
let transfo g c = 
  
  let fm = min_cap (Some c) in

    let add_flot g e=
      let gpre = add_arc g e.src e.tgt (-fm) in
        add_arc gpre e.tgt e.src fm
    in
      let rec flux_evo gi p=
        match p with
        | [] -> gi
        | e::rest -> flux_evo (add_flot gi e) rest

  in 
    flux_evo g c 
;;


(* Applique l'algorithme de ford_fulkerson *)

let algo_ff g s p =
  let g_resi = resi_init g in 

  let rec boucle g_resi s p=
    match find_path g_resi s p with
      |None -> g_resi
      |Some c -> boucle (transfo g_resi c) s p
in
  boucle g_resi s p
;;

(* Génère un graphe de flots à partir d'un graphe résiduel *)
let flot_gen g g_resi =

  let up_flot g e acc = 

    new_arc g {e with lbl={flot=(e.lbl.capacite - acc);capacite = e.lbl.capacite}} in

    let search_flux e = 
      match find_arc g_resi e.src e.tgt with
      | None -> 0
      | Some a -> a.lbl
    in

      let flux_calc g e = up_flot g e ((search_flux e)) in

        e_fold g flux_calc g
;;

(* Génère un graphe de string à partir d'un graphe de flots *)
let flow_to_string g =
  gmap g (fun x -> (string_of_int x.flot) ^ "/" ^ (string_of_int x.capacite)) 
;;


(* Applique l'algorithme à partir d'un string graph et renvoie un string graph avec les flots *)
let ff_ope g s p =

  let g_resi = algo_ff g s p in

  let g_flot = flot_gen (flot_init g) g_resi in

  flow_to_string g_flot;;
;;


