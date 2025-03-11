module Tezos_micheline = Tezos_micheline
module Michelson = Michelson
module Oasis_core = Oasis_core
module If_suffix_rewriter = If_suffix_rewriter
open Core

let rec micheline_to_oasis_micheline
  (node : (unit, Michelson.Ast.Prim.t) Tezos_micheline.Micheline.node)
  : Oasis_core.Micheline.t
  =
  match node with
  | Int (_, z) -> Int (Z.to_string z)
  | String (_, s) -> String s
  | Bytes (_, b) -> Bytes (Bytes.to_string b)
  | Prim (_, prim, args, annots) ->
    Primitive
      { name = Michelson.Ast.Prim.to_string prim
      ; annotations = annots
      ; arguments = List.map ~f:micheline_to_oasis_micheline args
      }
  | Seq (_, nodes) -> Sequence (List.map ~f:micheline_to_oasis_micheline nodes)
;;

let rec oasis_micheline_to_micheline (t : Oasis_core.Micheline.t)
  : (unit, Michelson.Ast.Prim.t) Tezos_micheline.Micheline.node
  =
  match t with
  | Int s -> Int ((), Z.of_string s)
  | String s -> String ((), s)
  | Bytes s ->
    (* Adjust based on whether 's' is raw bytes or hex-encoded *)
    let bytes =
      try (* If 's' is hex-encoded *)
          Hex.to_bytes (`Hex s) with
      | Invalid_argument _ ->
        (* If 's' is raw bytes *)
        Bytes.of_string s
    in
    Bytes ((), bytes)
  | Primitive { name; annotations; arguments } ->
    let prim = Michelson.Ast.Prim.of_string name in
    let args = List.map ~f:oasis_micheline_to_micheline arguments in
    Prim ((), prim, args, annotations)
  | Sequence nodes ->
    let compiled_nodes = List.map ~f:oasis_micheline_to_micheline nodes in
    Seq ((), compiled_nodes)
;;

let rec strip_annotations
  (x : (unit, Michelson.Ast.Prim.t) Tezos_micheline.Micheline.node)
  : (unit, Michelson.Ast.Prim.t) Tezos_micheline.Micheline.node
  =
  (* Number of type arguments for (some) primitives, where we will strip annotations *)
  let prim_type_args : Michelson.Ast.Prim.t -> int option = function
    | I None | I Nil | I Empty_set | I Push | I Left | I Right -> Some 1
    | I Empty_map | I Empty_big_map | I Lambda -> Some 2
    (* _not_ "CONTRACT"! annot is important there *)
    (* but could include "SELF", maybe? *)
    | _ -> None
  in
  let rec strip_annots =
    let open Tezos_micheline.Micheline in
    function
    | Seq (l, s) -> Seq (l, List.map ~f:strip_annots s)
    | Prim (l, p, lst, _) -> Prim (l, p, List.map ~f:strip_annots lst, [])
    | x -> x
  in
  match x with
  | Seq (l, args) ->
    let args = List.map ~f:strip_annotations args in
    Seq (l, args)
  | Prim (l, p, args, annot) ->
    (match prim_type_args p with
     | Some n ->
       let type_args, args = List.split_n args n in
       (* strip annots from type args *)
       let type_args = List.map ~f:strip_annots type_args in
       (* recur into remaining args *)
       let args = List.map ~f:strip_annotations args in
       Prim (l, p, type_args @ args, annot)
     | None ->
       let args = List.map ~f:strip_annotations args in
       Prim (l, p, args, annot))
  | x -> x
;;

let optimise_micheline
  ?(strip_annots = true)
  (node : (unit, Michelson.Ast.Prim.t) Tezos_micheline.Micheline.node)
  : (unit, Michelson.Ast.Prim.t) Tezos_micheline.Micheline.node
  =
  let oasis_micheline = micheline_to_oasis_micheline node in
  let code = Oasis_core.Michelson.Of_micheline.instruction oasis_micheline in
  (* Optimise the code here *)
  let optimised_code =
    Oasis_core.Michelson_rewriter.run Oasis_core.Michelson_rewriter.simplify code
  in
  let oasis_micheline_list =
    Oasis_core.Michelson.To_micheline.instruction optimised_code
  in
  let oasis_micheline = Oasis_core.Micheline.Sequence oasis_micheline_list in
  let micheline = oasis_micheline_to_micheline oasis_micheline in
  let micheline =
    Tezos_micheline.Micheline.map_node
      (fun x -> x)
      (fun prim -> Michelson.Ast.Prim.of_string prim)
      (If_suffix_rewriter.optimize
         (Tezos_micheline.Micheline.map_node
            (fun x -> x)
            (fun prim -> Michelson.Ast.Prim.to_string prim)
            micheline))
  in
  if strip_annots then strip_annotations micheline else micheline
;;
