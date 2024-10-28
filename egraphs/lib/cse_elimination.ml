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

let graph_expr = EGraph.add_sexp graph expr

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

let cost_function score (sym, children) =
  let node_score =
    match Symbol.to_string sym with
    | "*" -> 5.
    | "/" -> 5.
    | "<<" -> 1.
    | "?a" -> 1.
    | "pow" -> 10.
    | _ -> 0.
  in
  node_score +. List.fold_left (fun acc vl -> acc +. score vl) 0. children
;;

let%test _ =
  EGraph.extract cost_function graph graph_expr
  = List [ Atom "*"; Atom "tmp"; Atom "tmp"; Atom "tmp" ]
;;
