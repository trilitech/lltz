(* Copyright 2022-2023 Morum LLC, 2019-2022 Smart Chain Arena LLC *)

open Utils
open Control
open Michelson_base.Type

type ty =
  | T0 of type0
  | T1 of type1 * ty
  | T2 of type2 * ty * ty
  | Record of row
  | Variant of row
  | Vector of ty list
  | Missing of string
[@@deriving eq, show {with_path = false}]

(* inviariant: not a singleton *)
and row = (string option * ty) Binary_tree.t
[@@deriving eq, show {with_path = false}]

type tys =
  | Stack_ok of ty
  | Stack_failed
[@@deriving eq, show {with_path = false}]

let mk_t1 t t1 = T1 (t, t1)

let mk_t2 t t1 t2 = T2 (t, t1, t2)

let mk_leaf l t = Binary_tree.Leaf (l, t)

let mk_node n1 n2 = Binary_tree.Node (n1, n2)

let t_record r = Record r

let t_variant r = Variant r

let t_vector ts = Vector ts

let rec lub t1 t2 =
  let open Option in
  match (t1, t2) with
  | T0 t, T0 u when equal_type0 t u -> Some (T0 t)
  | T1 (t, t1), T1 (u, u1) when equal_type1 t u -> mk_t1 t <$> lub t1 u1
  | T2 (t, t1, t2), T2 (u, u1, u2) when equal_type2 t u ->
      mk_t2 t <$> lub t1 u1 <*> lub t2 u2
  | Record r, Record s -> t_record <$> lub_row true r s
  | Record r, s -> t_record <$> lub_row true r (Leaf (None, s))
  | r, Record s -> t_record <$> lub_row true (Leaf (None, r)) s
  | Variant r, Variant s -> t_variant <$> lub_row false r s
  | Vector ts1, Vector ts2 ->
      if List.length ts1 = List.length ts2
      then t_vector <$> Option.map2_list lub ts1 ts2
      else None
  | Missing s1, Missing s2 when s1 = s2 -> Some t1
  | _ -> None

and lub_row is_record r1 r2 =
  let open Option in
  match (r1, r2) with
  | Leaf (Some l1, t1), Leaf (Some l2, t2) when l1 = l2 ->
      mk_leaf (Some l1) <$> lub t1 t2
  | Leaf (Some l, t1), Leaf (None, t2) -> mk_leaf (Some l) <$> lub t1 t2
  | Leaf (None, t1), Leaf (Some l, t2) -> mk_leaf (Some l) <$> lub t1 t2
  | Leaf (_, t1), Leaf (_, t2) -> mk_leaf None <$> lub t1 t2
  | Node (r1, r2), Node (s1, s2) ->
      mk_node <$> lub_row is_record r1 s1 <*> lub_row is_record r2 s2
  | Leaf (_, Record r1), _ when is_record -> lub_row is_record r1 r2
  | _, Leaf (_, Record r2) when is_record -> lub_row is_record r1 r2
  | Leaf (None, Variant r1), Node _ when not is_record ->
      lub_row is_record r1 r2
  | Node _, Leaf (None, Variant r2) when not is_record ->
      lub_row is_record r1 r2
  | _ -> None

let lubs s1 s2 =
  let open Option in
  match (s1, s2) with
  | Stack_ok ts1, Stack_ok ts2 -> (fun x -> Stack_ok x) <$> lub ts1 ts2
  | Stack_failed, t2 -> Some t2
  | t1, Stack_failed -> Some t1

let compatible t1 t2 = Option.is_some (lub t1 t2)

let t_unit = T0 Unit

let t_missing s = Missing s

let t_nat = T0 Nat

let t_int = T0 Int

let t_mutez = T0 Mutez

let t_string = T0 String

let t_bytes = T0 Bytes

let t_bls12_381_g1 = T0 Bls12_381_g1

let t_bls12_381_g2 = T0 Bls12_381_g2

let t_bls12_381_fr = T0 Bls12_381_fr

let t_chain_id = T0 Chain_id

let t_timestamp = T0 Timestamp

let t_address = T0 Address

let t_key = T0 Key

let t_key_hash = T0 Key_hash

let t_signature = T0 Signature

let t_operation = T0 Operation

let t_sapling_state memo = T0 (Sapling_state {memo})

let t_sapling_transaction memo = T0 (Sapling_transaction {memo})

let t_never = T0 Never

let t_list t = T1 (List, t)

let t_set t = T1 (Set, t)

let t_contract t = T1 (Contract, t)

let t_ticket t = T1 (Ticket, t)

let t_map k v = T2 (Map, k, v)

let t_big_map k v = T2 (Big_map, k, v)

let t_lambda a b = T2 (Lambda, a, b)

let t_variant_node r1 r2 = Variant (Node (r1, r2))

let t_record_node r1 r2 = Record (Node (r1, r2))

let r_or t1 t2 = Binary_tree.Node (Leaf (None, t1), Leaf (None, t2))

let t_or t1 t2 = Variant (r_or t1 t2)

let leaf ?lbl t = Binary_tree.Leaf (lbl, t)

let t_pair ?annot1 ?annot2 t1 t2 =
  t_record_node (Leaf (annot1, t1)) (Leaf (annot2, t2))

let r_option t = Binary_tree.Node (leaf ~lbl:"None" t_unit, leaf ~lbl:"Some" t)

let t_option t = Variant (r_option t)

let t_bool = T0 Bool

let rec print_row ~sep =
  let rec f protect ppf : row -> _ = function
    | Leaf (None, t) -> Format.fprintf ppf "%a" print_ty t
    | Leaf (Some s, t) -> Format.fprintf ppf "%s : %a" s print_ty t
    | Node (r1, r2) ->
        let r ppf = Format.fprintf ppf "%a %s %a" (f true) r1 sep (f true) r2 in
        if protect then Format.fprintf ppf "(%t)" r else r ppf
  in
  f false

and print_ty ppf = function
  | Record r -> Format.fprintf ppf "{ %a }" (print_row ~sep:";") r
  | Variant r -> Format.fprintf ppf "< %a >" (print_row ~sep:"|") r
  | Vector ts -> Format.fprintf ppf "%a" (List.pp print_ty) ts
  | T0 t ->
      let t, memo = string_of_type0 t in
      Format.fprintf ppf "%s" (t ^ Option.cata "" (flip ( ^ ) " ") memo)
  | T1 (t, t1) -> Format.fprintf ppf "%s(%a)" (string_of_type1 t) print_ty t1
  | T2 (t, t1, t2) ->
      let t, a1, a2 = string_of_type2 t in
      let a1 = Option.cata "" (fun x -> "%" ^ x) a1 in
      let a2 = Option.cata "" (fun x -> "%" ^ x) a2 in
      Format.fprintf ppf "%s(%a%s, %a%s)" t print_ty t1 a1 print_ty t2 a2
  | Missing s -> Format.fprintf ppf "missing(%S)" s

let print_tys ppf = function
  | Stack_ok t -> Format.fprintf ppf "%a" print_ty t
  | Stack_failed -> Format.fprintf ppf "failed"

let get1 = function
  | Stack_ok t -> Ok t
  | Stack_failed -> Error "get1: failed stack"

let unleaf_variant =
  Binary_tree.(
    function
    | Leaf (_, t) -> t
    | Node _ as t -> Variant t)

let view_option =
  let open Binary_tree in
  function
  | Variant (Node (Leaf (Some "None", T0 Unit), Leaf (Some "Some", t))) ->
      Some t
  | _ -> None

let view_or =
  let open Binary_tree in
  function
  | Variant (Node (tl, tr)) -> Some (unleaf_variant tl, unleaf_variant tr)
  | _ -> None
