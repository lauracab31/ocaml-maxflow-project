open Gfile
open Jobs
    
let () =

  (* Check the number of command-line arguments *)
  if Array.length Sys.argv <> 5 then
    begin
      Printf.printf
        "\n ✻  Usage: %s infile source sink outfile\n\n%s%!" Sys.argv.(0)
        ("    🟄  infile  : input file containing a graph\n" ^
         "    🟄  source  : identifier of the source vertex (used by the ford-fulkerson algorithm)\n" ^
         "    🟄  sink    : identifier of the sink vertex (ditto)\n" ^
         "    🟄  outfile : output file in which the result should be written.\n\n") ;
      exit 0
    end ;


  (* Arguments are : infile(1) source-id(2) sink-id(3) outfile(4) *)
  
  let infile = Sys.argv.(1)
  and outfile = Sys.argv.(4)
  
  (* These command-line arguments are not used for the moment. *)
  and _source = int_of_string Sys.argv.(2)
  and _sink = int_of_string Sys.argv.(3)
  in

  (* Open file *)
  let graph1 = jobs_to_record infile in
  let gf = fst (record_to_graph graph1) in
  (*let graph2 = gmap graph1 int_of_string in

  let graph3 = add_arc graph2 0 1 1 in 

  let graph4 = gmap graph3 string_of_int in*)
  (* Rewrite the graph that has been read. *)
  let () = export outfile gf in                     (*export outfile graph1 in*)

  ()

  

