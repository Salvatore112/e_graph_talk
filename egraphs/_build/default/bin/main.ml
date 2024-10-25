open Ego.Basic

(* E-graph initialization *)
let graph = EGraph.init ()

let expr =
  let open Sexplib0.Sexp in
  List [ Atom "/"; List [ Atom "*"; Atom "a"; Atom "2" ]; Atom "2" ]
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

(* a * 2 = a >> 1 *)

add_rule
  (List [ Atom "*"; Atom "?a"; Atom "2" ])
  (List [ Atom ">>"; Atom "?a"; Atom "1" ])
;;

(* (a * b) / c = a * (b / c) *)
add_rule
  (List [ Atom "/"; List [ Atom "*"; Atom "?a"; Atom "?b" ]; Atom "?c" ])
  (List [ Atom "*"; Atom "?a"; List [ Atom "/"; Atom "?b"; Atom "?c" ] ])
;;

(* a / a = 1 *)
add_rule (List [ Atom "/"; Atom "?a"; Atom "?a" ]) (List [ Atom "1" ]);;

(* a * 1 = a *)
add_rule (List [ Atom "*"; Atom "?a"; Atom "1" ]) (Atom "?a")

(* E-graph saturation *)

let g : Odot.graph = EGraph.to_dot graph
let () = Core.Out_channel.write_all "demo_egraph.dot" ~data:Odot.(string_of_graph g)
