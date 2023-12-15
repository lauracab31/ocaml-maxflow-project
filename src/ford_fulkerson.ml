open Graph
open Tools

type flot_node ={

  flot : int ;

  capacite : int ;

}

let min_cap l =
  let rec min_capb l min = match l with
  |None | Some([]) -> min
  |Some (x::rest) -> if x.lbl < min then min_capb (Some rest) x.lbl else min_capb (Some rest) min
  
  in
  min_capb l 99999999
;;

let flot_init g = gmap g (fun x -> {flot = 0 ; capacite = int_of_string x});;

let resi_init g = gmap g (fun x -> int_of_string x);;


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


let algo_ff g s p =
  let g_resi = resi_init g in 

  let rec boucle g_resi s p=
    match find_path g_resi s p with
      |None -> g_resi
      |Some c -> boucle (transfo g_resi c) s p
in
  boucle g_resi s p
;;

let flot_gen g g_resi =

  let up_flot g e acc = new_arc g {e with lbl={flot=(e.lbl.flot - acc);capacite = e.lbl.capacite}} in

    let search_flux e = 
      match find_arc g_resi e.src e.tgt with
      | None -> 0
      | Some a -> a.lbl
    in

      let flux_calc g e = up_flot g e ((search_flux e) - (search_flux {e with src = e.tgt ; tgt = e.src})) in

        e_fold g flux_calc g
  ;;
    

