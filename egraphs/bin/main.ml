open Ego.Basic

let graph = EGraph.init ()

(* (15 ** 17) * (15 ** 17) * (15 ** 17) *)
let expr =
  let open Sexplib0.Sexp in
  List
    [ Atom "*"
    ; List [ Atom "pow"; List [ Atom "15"; Atom "17" ] ]
    ; List [ Atom "pow"; List [ Atom "15"; Atom "17" ] ]
    ; List [ Atom "pow"; List [ Atom "15"; Atom "17" ] ]
    ]
;;

let _ = EGraph.add_sexp graph expr

(* Rewrites definition *)

let add_rule from_list into_list =
  let from = Query.of_sexp from_list in
  let into = Query.of_sexp into_list in
  let rule = Rule.make ~from ~into in
  let _ =
    match rule with
    | Some r -> EGraph.run_until_saturation graph [ r ]
    | None -> failwith "a"
  in
  ()
;;

(* Let's eliminate 15 ** 17 *)

(* tmp = 15 ** 17 *)
add_rule (List [ Atom "pow"; List [ Atom "15"; Atom "17" ] ]) (Atom "tmp")

let g : Odot.graph = EGraph.to_dot graph

let () =
  let out_channel = open_out "test.dot" in
  output_string out_channel (Odot.string_of_graph g);
  close_out out_channel
;;
