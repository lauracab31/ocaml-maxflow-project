open Graph

let clone_nodes gr = n_fold gr new_node empty_graph;;

let gmap gr f =e_fold gr (fun g a -> new_arc g {a with lbl= (f a.lbl)}) (clone_nodes gr) ;;

let add_arc g id1 id2 n = match find_arc g id1 id2 with
  |None -> new_arc g {src=id1 ; tgt=id2 ; lbl=n}
  |Some arc -> new_arc g {src=id1 ; tgt=id2 ; lbl=arc.lbl+n};;


let find_path g id1 id2 =


  let rec node_descent g id1 id2 acc =
    let path_list = List.filter(fun x->x.lbl>0) (out_arcs g id1) in 
    
      let rec search id acc = function
        |[] -> None
        |p::rest -> if (p.tgt = id) then Some (List.rev (p::acc)) else if not(List.exists (fun x->x=p) acc) then 
          match node_descent g p.tgt id2 (p::acc) with
            | None -> search id acc rest
            | Some res -> Some res
          else search id acc rest
      in 

      search id2 acc path_list
    in

  node_descent g id1 id2 []
;;

let rec print_arclist = function
 |None -> Printf.printf " "
 |Some (x :: rest) -> Printf.printf "[%d -> %d]%!" x.src x.tgt ; print_arclist (Some rest)
 |Some([]) -> Printf.printf "\n %!"

;;

let rec nb_nodes g acu= 
  match node_exists g acu with
    | true -> nb_nodes g (acu+1)
    | false -> acu
;;