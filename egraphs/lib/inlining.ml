open Ego.Basic

(* E-graph initialization *)
let graph = EGraph.init ()

(* 1 + max(50, 60) *)
let expr =
  let open Sexplib0.Sexp in
  List [ Atom "+"; Atom "1"; List [ Atom "max"; Atom "50"; Atom "60" ] ]
;;

(* int max(int x, int y)
{
    if (x > y)
        return x;
    else
        return y;
}
*)

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

(* inline max(x,y) *)
add_rule
  (List [ Atom "max"; Atom "?a"; Atom "?b" ])
  (List [ Atom "if"; List [ Atom ">"; Atom "?a"; Atom "?b" ]; Atom "?a"; Atom "?b" ])

(* E-graph saturation *)

let cost_function score (sym, children) =
  let node_score =
    match Symbol.to_string sym with
    | "*" -> 5.
    | "/" -> 5.
    | "?a" -> 1.
    | "max" -> 15.
    | _ -> 0.
  in
  node_score +. List.fold_left (fun acc vl -> acc +. score vl) 0. children
;;

let%test _ =
  EGraph.extract cost_function graph graph_expr
  = List
      [ Atom "+"
      ; Atom "1"
      ; List [ Atom "if"; List [ Atom ">"; Atom "50"; Atom "60" ]; Atom "50"; Atom "60" ]
      ]
;;
