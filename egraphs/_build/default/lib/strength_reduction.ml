open Ego.Basic

(* E-graph initialization *)
let graph = EGraph.init ()

let expr =
  let open Sexplib0.Sexp in
  List
    [ Atom "if"
    ; List [ Atom "%"; Atom "a"; Atom "2" ]
    ; List [ Atom "*"; Atom "a"; Atom "2" ]
    ; List [ Atom "/"; Atom "a"; Atom "2" ]
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

(* a * 2 = a << 1 *)
add_rule
  (List [ Atom "*"; Atom "?a"; Atom "2" ])
  (List [ Atom "<<"; Atom "?a"; Atom "1" ])
;;

(* a / 2 = a >> 1 *)
add_rule
  (List [ Atom "/"; Atom "?a"; Atom "2" ])
  (List [ Atom ">>"; Atom "?a"; Atom "1" ])
;;

(* a % 2 = a & 1 *)
add_rule (List [ Atom "%"; Atom "?a"; Atom "2" ]) (List [ Atom "&"; Atom "?a"; Atom "1" ])

(* E-graph saturation *)

let cost_function score (sym, children) =
  let node_score =
    match Symbol.to_string sym with
    | "*" -> 5.
    | "/" -> 5.
    | "%" -> 5.
    | ">>" -> 1.
    | "<<" -> 1.
    | "?a" -> 1.
    | "&" -> 1.
    | _ -> 0.
  in
  node_score +. List.fold_left (fun acc vl -> acc +. score vl) 0. children
;;

let%test _ =
  EGraph.extract cost_function graph graph_expr
  = List
      [ Atom "if"
      ; List [ Atom "&"; Atom "a"; Atom "1" ]
      ; List [ Atom "<<"; Atom "a"; Atom "1" ]
      ; List [ Atom ">>"; Atom "a"; Atom "1" ]
      ]
;;
