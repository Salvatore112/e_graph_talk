open Ego.Basic

let graph = EGraph.init ()

(* (a + 0) * 1 - 0 / 1 *)
let expr =
  let open Sexplib0.Sexp in
  List
    [ Atom "-"
    ; List [ Atom "*"; List [ Atom "+"; Atom "a"; Atom "0" ]; Atom "1" ]
    ; List [ Atom "/"; Atom "0"; Atom "1" ]
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

(* a / 1 = a *)
add_rule (List [ Atom "/"; Atom "?a"; Atom "1" ]) (Atom "?a");;

(* a - 0 = a *)
add_rule (List [ Atom "-"; Atom "?a"; Atom "0" ]) (Atom "?a");;

(* a * 1 = a *)
add_rule (List [ Atom "*"; Atom "?a"; Atom "1" ]) (Atom "?a");;

(* a + 0 = a *)
add_rule (List [ Atom "+"; Atom "?a"; Atom "0" ]) (Atom "?a")

let cost_function score (sym, children) =
  let node_score =
    match Symbol.to_string sym with
    | "*" -> 1.
    | "/" -> 1.
    | "<<" -> 5.
    | "?a" -> 10.
    | _ -> 0.
  in
  node_score +. List.fold_left (fun acc vl -> acc +. score vl) 0. children
;;

let%test _ = EGraph.extract cost_function graph graph_expr = Atom "a"
