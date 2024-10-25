open Ego.Basic

let graph = EGraph.init ()

(* (a + 0) * 1 - 0 / 1 *)
let expr =
  let open Sexplib0.Sexp in
  List
    [ Atom "if"
    ; List [ Atom "%"; Atom "x"; Atom "2" ]
    ; List [ Atom "/"; Atom "x"; Atom "2" ]
    ; List [ Atom "*"; Atom "x"; Atom "2" ]
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

(* a % 2 = a & 1*)
add_rule (List [ Atom "%"; Atom "x"; Atom "2" ]) (List [ Atom "&"; Atom "x"; Atom "1" ])

(* a / 2 = a >> 1 *)
(* add_rule
   (List [ Atom "/"; Atom "?a"; Atom "2" ])
   (List [ Atom ">>"; Atom "?a"; Atom "1" ])
   ;;

   (* a * 2 = a << 1*)
   add_rule
   (List [ Atom "*"; Atom "?a"; Atom "2" ])
   (List [ Atom "<<"; Atom "?a"; Atom "1" ]) *)

let g : Odot.graph = EGraph.to_dot graph
let () = Core.Out_channel.write_all "awfwf.dot" ~data:Odot.(string_of_graph g)
