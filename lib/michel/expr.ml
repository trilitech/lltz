(* Copyright 2022-2023 Morum LLC, 2019-2022 Smart Chain Arena LLC *)

open Utils
open Control
open Type
open Format
include Michelson_base.Primitive

type lit =
  | Unit
  | Bool of bool
  | Nat of Bigint.t
  | Int of Bigint.t
  | Mutez of Bigint.t
  | String of string
  | Key of string
  | Key_hash of string
  | Bytes of string
  | Chain_id of string
  | Address of string
  | Timestamp of string
  | Bls12_381_g1 of string
  | Bls12_381_g2 of string
  | Bls12_381_fr of string
  | Signature of string
[@@deriving eq, show {with_path = false}]

type stack_op =
  | Swap
  | Dup of int
  | Dig of int
  | Dug of int
  | Drop of int
[@@deriving eq, show {with_path = false}]

type row_context = (string option * ty) Binary_tree.context
[@@deriving eq, show {with_path = false}]

type record_pattern = string option Binary_tree.t
[@@deriving eq, show {with_path = false}]

type pattern =
  | P_var of string option
  | P_vector of string option list
[@@deriving eq, show {with_path = false}]

type 'e match_clause = {
    cons : string option
  ; var : string option
  ; rhs : 'e
}
[@@deriving eq, map, fold, show {with_path = false}]

type 'e precontract = {
    tparameter : ty * string option
  ; tstorage : ty
  ; parameter_and_storage : string option
  ; body : 'e
}
[@@deriving eq, map, fold, show {with_path = false}]

type 'e expr_f =
  | Var of string
  | Let_in of pattern * 'e * 'e
  | Lambda of string option * ty * ty * 'e
  | Lambda_rec of string option * string option * ty * ty * 'e
  | Lit of lit
  (* Primitives: *)
  | Prim0 of ty prim0
  | Prim1 of ty prim1 * 'e
  | Prim1_fail of prim1_fail * 'e
  | Prim2 of ty prim2 * 'e * 'e
  | Prim3 of prim3 * 'e * 'e * 'e
  | Proj_field of string * 'e
  | Stack_op of stack_op * 'e list
  | Record of (string option * 'e) Binary_tree.t
  | Variant of string option * row_context * 'e
  | List of ty * 'e list
  | Set of ty * 'e list
  | Map of ty * ty * ('e * 'e) list
  (* Control structures: *)
  | Match_record of record_pattern * 'e * 'e
  | Match_variant of 'e * 'e match_clause Binary_tree.t
  | Vector of 'e list
  | Nth of int * 'e
  | Unpair of int * 'e
  | If of 'e * 'e * 'e
  | If_none of 'e * string option * 'e * 'e
  | If_left of 'e * string option * 'e * string option * 'e
  | If_cons of 'e * string option * string option * 'e * 'e
  | Loop of 'e list * string option list * 'e
  | Iter_over of 'e list * string option list * 'e
  | Map_over of 'e list * string option list * 'e
  | Create_contract of 'e precontract * 'e * 'e * 'e
  | Record_of_tree of string option Binary_tree.t * 'e
  | Comment of string list * 'e
[@@deriving eq, map, fold, show {with_path = false}]

(* Workaround for ppx_deriving-generated equality. Without this
   re-definition equality on large expressions can result in a stack
   overflow. *)
(*let equal_expr_f eq x y =
  match (x, y) with
  | Let_in (xs, e1, e2), Let_in (xs', e1', e2') ->
      equal_pattern xs xs' && eq e1 e1' && eq e2 e2'
  | x, y -> equal_expr_f eq x y*)

type texpr = {
    texpr : texpr expr_f
  ; tys : tys
}
[@@deriving eq, show {with_path = false}]

type expr = {expr : expr expr_f} [@@deriving eq, show {with_path = false}]

type env = (string * ty) list

module Traversable (A : APPLICATIVE) = struct
  module BT = Binary_tree.Traversable (A)

  open Applicative (A)
  let sequenceA = function
    | Var _ as e -> return e
    | Let_in (rp, e1, e2) ->
        let+ e1 = e1
        and+ e2 = e2 in
        Let_in (rp, e1, e2)
    | Lambda (rp, a, b, e) ->
        let+ e = e in
        Lambda (rp, a, b, e)
    | Lambda_rec (rp, f, a, b, e) ->
        let+ e = e in
        Lambda_rec (rp, f, a, b, e)
    | Lit _ as e -> return e
    | Prim0 p -> return (Prim0 p)
    | Prim1 (p, e1) ->
        let+ e1 = e1 in
        Prim1 (p, e1)
    | Prim1_fail (p, e1) ->
        let+ e1 = e1 in
        Prim1_fail (p, e1)
    | Prim2 (p, e1, e2) ->
        let+ e1 = e1
        and+ e2 = e2 in
        Prim2 (p, e1, e2)
    | Prim3 (p, e1, e2, e3) ->
        let+ e1 = e1
        and+ e2 = e2
        and+ e3 = e3 in
        Prim3 (p, e1, e2, e3)
    | Proj_field (fld, x) ->
        let+ x = x in
        Proj_field (fld, x)
    | Stack_op (op, xs) ->
        let+ xs = sequence_list xs in
        Stack_op (op, xs)
    | Record r ->
        let+ r = BT.map (fun (x, y) -> pair x <$> y) r in
        Record r
    | Variant (lbl, rc, e) ->
        let+ e = e in
        Variant (lbl, rc, e)
    | List (ty, es) ->
        let+ es = sequence_list es in
        List (ty, es)
    | Set (ty, es) ->
        let+ es = sequence_list es in
        Set (ty, es)
    | Map (ty1, ty2, xs) ->
        let+ xs = map_list (fun (x, y) -> pair <$> x <*> y) xs in
        Map (ty1, ty2, xs)
    | Match_record (rp, e1, e2) ->
        let+ e1 = e1
        and+ e2 = e2 in
        Match_record (rp, e1, e2)
    | Match_variant (e, clauses) ->
        let+ e = e
        and+ clauses =
          BT.map
            (fun {cons; var; rhs} ->
              let+ rhs = rhs in
              {cons; var; rhs})
            clauses
        in
        Match_variant (e, clauses)
    | Vector es ->
        let+ es = sequence_list es in
        Vector es
    | Nth (n, v) ->
        let+ v = v in
        Nth (n, v)
    | Unpair (n, v) ->
        let+ v = v in
        Unpair (n, v)
    | If (e1, e2, e3) ->
        let+ e1 = e1
        and+ e2 = e2
        and+ e3 = e3 in
        If (e1, e2, e3)
    | If_none (e1, x, e2, e3) ->
        let+ e1 = e1
        and+ e2 = e2
        and+ e3 = e3 in
        If_none (e1, x, e2, e3)
    | If_cons (e1, x1, x2, e2, e3) ->
        let+ e1 = e1
        and+ e2 = e2
        and+ e3 = e3 in
        If_cons (e1, x1, x2, e2, e3)
    | If_left (e1, x2, e2, x3, e3) ->
        let+ e1 = e1
        and+ e2 = e2
        and+ e3 = e3 in
        If_left (e1, x2, e2, x3, e3)
    | Loop (init, xs, step) ->
        let+ init = sequence_list init
        and+ step = step in
        Loop (init, xs, step)
    | Map_over (init, xs, step) ->
        let+ init = sequence_list init
        and+ step = step in
        Map_over (init, xs, step)
    | Iter_over (init, xs, step) ->
        let+ init = sequence_list init
        and+ step = step in
        Iter_over (init, xs, step)
    | Create_contract
        ({tparameter; tstorage; parameter_and_storage; body}, e1, e2, e3) ->
        let+ body = body
        and+ e1 = e1
        and+ e2 = e2
        and+ e3 = e3 in
        Create_contract
          ({tparameter; tstorage; parameter_and_storage; body}, e1, e2, e3)
    | Record_of_tree (lbls, e) ->
        let+ e = e in
        Record_of_tree (lbls, e)
    | Comment (c, e) ->
        let+ e = e in
        Comment (c, e)
end

(*
let rec cata_expr f {expr} = f (map_expr_f (cata_expr f) expr)

let rec para_expr f {expr} = ({expr}, f (map_expr_f (para_expr f) expr))

let para_expr f e = snd (para_expr f e)

let rec cata_texpr f {texpr; tys} =
  (f (map_expr_f (cata_texpr f) texpr) tys, tys)

let cata_texpr f x = fst (cata_texpr f x)

let rec para_texpr f {texpr; tys} =
  ({texpr; tys}, f (map_expr_f (para_texpr f) texpr))

let para_texpr f e = snd (para_texpr f e)

let erase_types x = cata_texpr (fun e _ -> {expr = map_expr_f fst e}) x
*)

let print_lit ppf = function
  | Unit -> fprintf ppf "()"
  | Int i -> fprintf ppf "%a" Bigint.pp i
  | Nat i -> fprintf ppf "%a#nat" Bigint.pp i
  | Mutez i -> fprintf ppf "%a#mutez" Bigint.pp i
  | String s -> fprintf ppf "%S" s
  | Key s -> fprintf ppf "%S#key" s
  | Key_hash s -> fprintf ppf "%S#key_hash" s
  | Bytes s -> fprintf ppf "%S#bytes" s
  | Chain_id s -> fprintf ppf "%S#chain_id" s
  | Address s -> fprintf ppf "%S#address" s
  | Timestamp s -> fprintf ppf "%S#timestamp" s
  | Bool true -> fprintf ppf "True"
  | Bool false -> fprintf ppf "False"
  | Bls12_381_g1 s -> fprintf ppf "%S#bls12_381_g1" s
  | Bls12_381_g2 s -> fprintf ppf "%S#bls12_381_g2" s
  | Bls12_381_fr s -> fprintf ppf "%S#bls12_381_fr" s
  | Signature s -> fprintf ppf "%S#signature" s

let print_var ppf = function
  | None -> fprintf ppf "_"
  | Some x -> fprintf ppf "%s" x

let print_vars =
  pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf "; ") print_var

let print_prim0 ppf = function
  | Unit_ -> fprintf ppf "Unit"
  | None_ t -> fprintf ppf "None<%a>" print_ty t
  | Nil t -> fprintf ppf "Nil<%a>" print_ty t
  | Empty_set t -> fprintf ppf "Empty_set<%a>" print_ty t
  | Empty_map (t1, t2) -> fprintf ppf "Empty_set<%a,%a>" print_ty t1 print_ty t2
  | Empty_bigmap (t1, t2) ->
      fprintf ppf "Empty_set<%a,%a>" print_ty t1 print_ty t2
  | x -> pp_prim0 print_ty ppf x

let print_prim1 ppf = function
  | x -> pp_prim1 ppf x

let print_prim1_fail ppf = function
  | x -> pp_prim1_fail ppf x

let print_prim2 ppf = function
  | Pair (None, None) -> fprintf ppf "Pair"
  | Pair (t1, t2) ->
      let pp ppf x = fprintf ppf "%s" (Option.default "_" x) in
      fprintf ppf "%s<%a,%a>" "Pair" pp t1 pp t2
  | x -> pp_prim2 print_ty ppf x

let print_prim3 ppf = function
  | x -> pp_prim3 ppf x

let print_pattern ppf = function
  | P_var x -> fprintf ppf "%a" print_var x
  | P_vector xs -> fprintf ppf "%a" (List.pp print_var) xs

let print_record_pattern =
  let open Binary_tree in
  let rec f protect ppf = function
    | Leaf x -> fprintf ppf "%a" print_var x
    | Node (r1, r2) ->
        let r ppf = fprintf ppf "%a@,, %a" (f true) r1 (f false) r2 in
        if protect
        then fprintf ppf "@[<hv>( %t@ )@]" r
        else fprintf ppf "@[<hv>%t@]" r
  in
  f false

let rec print_rowish pre post ppf = function
  | Binary_tree.Leaf x -> x ppf
  | Node (r1, r2) ->
      let pr = print_rowish "" "" in
      let prp = print_rowish "( " " )" in
      fprintf ppf "@[<hv>%s%a@,; %a%s@]" pre prp r1 pr r2 post

let rec print_row_context pre post ppf =
  let pr = print_rowish "" "" in
  let prc = print_row_context "" "" in
  let prp = print_rowish "(" ")" in
  let prcp = print_row_context "(" ")" in
  function
  | Binary_tree.Hole -> fprintf ppf "."
  | Node_left (c, t) ->
      fprintf ppf "@[<hv>%s %a@,; %a@ %s@]" pre prcp c pr t post
  | Node_right (t, c) ->
      fprintf ppf "@[<hv>%s %a@,; %a@ %s@]" pre prp t prc c post

let print_vector es ppf =
  match es with
  | [] -> fprintf ppf "[]"
  | _ ->
      let pp_sep ppf () = fprintf ppf "@,; " in
      fprintf ppf "@[<hv>[ ";
      pp_print_list ~pp_sep (fun ppf f -> f ppf) ppf es;
      fprintf ppf "@;]@]"

let _print_app ppf pp_op op es =
  let pp_sep ppf () = fprintf ppf "@;<0 2>, " in
  fprintf ppf "@[<hv>%a@;<0 2>( " pp_op op;
  pp_print_list ~pp_sep (fun ppf f -> f ppf) ppf es;
  fprintf ppf "@;<1 2>)@]"

let print_app ppf pp_op op es =
  let pp_sep ppf () = fprintf ppf "@;<0 2>, " in
  fprintf ppf "@[<hv>%a" pp_op op;
  pp_print_custom_break ppf ~fits:("", 0, "(") ~breaks:("", 2, "( ");
  pp_print_list ~pp_sep (fun ppf f -> f ppf) ppf es;
  pp_print_custom_break ppf ~fits:(")", 0, "") ~breaks:("", 2, ")");
  fprintf ppf "@]"

let print_precontract pp ppf {tparameter; tstorage; parameter_and_storage; body}
    =
  fprintf ppf "@[<v>";
  (match tparameter with
  | t, None -> fprintf ppf "parameter :  %a@;" print_ty t
  | t, Some annot -> fprintf ppf "parameter :  %a %%%s@;" print_ty t annot);
  fprintf ppf "storage   :  %a@;" print_ty tstorage;
  fprintf ppf "%a ->@;%a" print_var parameter_and_storage pp body;
  fprintf ppf "@]"

let rec unroll_lets r = function
  | {expr = Let_in (vs, e1, e2)} -> unroll_lets ((vs, e1) :: r) e2
  | e -> (List.rev r, e)

let unroll_lets = unroll_lets []

(*
let rec print_expr ppf e =
  let open Format in
  match e.expr with
  | Let_in _ as expr ->
      let bs, r = unroll_lets {expr} in
      let pp = print_pattern in
      pp_open_vbox ppf 0;
      let f (vs, e) =
        fprintf ppf "@[<hv>let %a =@;<1 2>%a@]@," pp vs print_expr e
      in
      List.iter f bs;
      fprintf ppf "%a" print_expr r;
      pp_close_box ppf ()
  | e -> (
      let e = map_expr_f (fun ppf e -> print_expr e ppf) e in
      match e with
      | Let_in _ -> assert false
      | Var s -> fprintf ppf "%s" s
      | Lambda (rp, a, b, e) ->
          let pp = print_ty in
          fprintf ppf "(fun (%a : %a) : %a ->@ %t)" print_var rp pp a pp b e
      | Lambda_rec (rp, f, a, b, e) ->
          let pp = print_ty in
          fprintf ppf "(fun rec (%a : %a) %a : %a ->@ %t)" print_var rp pp a
            print_var f pp b e
      | Lit l -> print_lit ppf l
      | Prim0 op -> fprintf ppf "%a" print_prim0 op
      | Prim1 (Contract (None, t), e) ->
          fprintf ppf "Contract(%t, %a)" e print_ty t
      | Prim1 (Contract (Some ep, t), e) ->
          fprintf ppf "Contract(%t%%%s, %a)" e ep print_ty t
      | Prim1 (op, e) -> fprintf ppf "%a(%t)" (print_prim1 print_ty) op e
      | Prim1_fail (op, e) -> fprintf ppf "%a(%t)" print_prim1_fail op e
      | Prim2 (op, e1, e2) -> print_app ppf print_prim2 op [e1; e2]
      | Prim3 (op, e1, e2, e3) ->
          fprintf ppf "%a(%t, %t, %t)" print_prim3 op e1 e2 e3
      | Proj_field (fld, e) -> fprintf ppf "%t.%s" e fld
      | Create_contract (c, e1, e2, e3) ->
          let pp = print_precontract (fun ppf -> fprintf ppf "%t") in
          fprintf ppf "create_contract(%a, %t, %t, %t)" pp c e1 e2 e3
      | Stack_op (op, xs) ->
          fprintf ppf "%a(%t)" pp_stack_op op (print_vector xs)
      | Record r ->
          let f = function
            | None, rhs -> rhs
            | Some lhs, rhs -> fun ppf -> fprintf ppf "%s = %t" lhs rhs
          in
          fprintf ppf "@[<hv>{ %a }@]" (print_rowish "" "")
            (Binary_tree.map f r)
      | Variant (cons, rc, e) ->
          let f = function
            | None, t -> fun ppf -> print_ty ppf t
            | Some lhs, t -> fun ppf -> fprintf ppf "%s : %a" lhs print_ty t
          in
          let cons = Option.default "_" cons in
          fprintf ppf "%s#%a(%t)" cons
            (print_row_context "<" ">")
            (Binary_tree.Context.map f rc)
            e
      | List (t, xs) -> fprintf ppf "[%t]#list(%a)" (print_list xs) print_ty t
      | Set (t, xs) -> fprintf ppf "[%t]#set(%a)" (print_list xs) print_ty t
      | Map (k, v, xs) ->
          let xs ppf =
            pp_print_list
              ~pp_sep:(fun ppf () -> fprintf ppf "; ")
              (fun ppf (k, v) -> fprintf ppf "%t: %t" k v)
              ppf xs
          in
          fprintf ppf "[%t]#map(%a,%a)" xs print_ty k print_ty v
      | Match_record (vs, e1, e2) ->
          let pp = print_record_pattern in
          fprintf ppf "@[<v>@[<hv>let record %a =@;<1 2>%t@]@,%t@]" pp vs e1 e2
      | Match_variant (scrutinee, clauses) ->
          let rec pp ppf = function
            | Binary_tree.Leaf {cons; var; rhs} ->
                let cons = Option.default "." cons in
                let var = Option.default "_" var in
                fprintf ppf "@[<hov>| %s %s ->@;<1 4>%t@]" cons var rhs
            | Binary_tree.Node (t1, t2) ->
                fprintf ppf "@[<v>%a@,%a@]" pp t1 pp t2
          in
          fprintf ppf "@[<v>match %t with@," scrutinee;
          fprintf ppf "%a" pp clauses;
          fprintf ppf "@,end@]"
      | Vector es -> print_vector es ppf
      | Nth (n, e) -> fprintf ppf "%t[%d]" e n
      | Unpair (n, e) -> fprintf ppf "Unpair(%d, %t)" n e
      | If (e1, e2, e3) ->
          fprintf ppf "@[<hv>if %t@ then@;<1 2>%t@ else@;<1 2>%t@]" e1 e2 e3
      | If_none (e1, x, e2, e3) ->
          let x = Option.default "_" x in
          fprintf ppf "@[<hv>if %t is Some %s@ then@;<1 2>%t@ else@;<1 2>%t@]"
            e1 x e3 e2
      | If_cons (e1, x, xs, e2, e3) ->
          let x = Option.default "_" x in
          let xs = Option.default "_" xs in
          fprintf ppf "@[<hv>if %t is %s :: %s@ then@;<1 2>%t@ else@;<1 2>%t@]"
            e1 x xs e2 e3
      | If_left (scrutinee, xl, e2, xr, e3) ->
          let pp ppf (cons, x, e) =
            let x = Option.default "_" x in
            fprintf ppf "@[<hov>| %s %s ->@;<1 2>%t@]" cons x e
          in
          fprintf ppf "@[<v>if_left %t with@," scrutinee;
          fprintf ppf "@[<v>%a@,%a@]" pp ("Left", xl, e2) pp ("Right", xr, e3);
          fprintf ppf "@,end@]"
      | Loop (init, xs, step)
      | Map_over (init, xs, step)
      | Iter_over (init, xs, step) ->
          let name =
            match e with
            | Loop _ -> "loop"
            | Map_over _ -> "map"
            | Iter_over _ -> "iter"
            | _ -> assert false
          in
          fprintf ppf "@[<v>%s %t@;<0 0>step %a ->@;<0 2>%t@]" name
            (print_vector init) print_vars xs step
      | Record_of_tree (_lbls, e) -> fprintf ppf "record_of_tree(..., %t)" e
      | Comment (c, e) ->
          fprintf ppf "@[<v>@[<hv>(* %s *)@;<1 0>%t@]@]" (String.concat " " c) e
      )
*)

let record xs = {expr = Record xs}

let tuple xs =
  {expr = Record (Binary_tree.right_comb (List.map (pair None) xs))}

let proj_field l e = {expr = Proj_field (l, e)}

let match_record rp e1 e2 = {expr = Match_record (rp, e1, e2)}

let match_variant cond clauses = {expr = Match_variant (cond, clauses)}

let loop rp step init = {expr = Loop (rp, step, init)}

let _print_ty_result ppf =
  Result.cata (fun x -> print_ty ppf x) (fun e -> fprintf ppf "Error: %s" e)

let prim0 op = {expr = Prim0 op}

let prim1 op x = {expr = Prim1 (op, x)}

let prim1_fail op x = {expr = Prim1_fail (op, x)}

let prim2 op x y = {expr = Prim2 (op, x, y)}

let prim3 op x y z = {expr = Prim3 (op, x, y, z)}

let stack_op op x = {expr = Stack_op (op, x)}

let lit l = {expr = Lit l}

let var x = {expr = Var x}

let michel_list t entries = {expr = List (t, entries)}

let michel_set t entries = {expr = Set (t, entries)}

let michel_map tk tv entries = {expr = Map (tk, tv, entries)}

let variant lbl rc x = {expr = Variant (lbl, rc, x)}

let unit = {expr = Lit Unit}

let rec row_context lbl =
  let open Binary_tree in
  let open Option in
  function
  | Leaf (Some l, _) -> if lbl = l then Some Hole else None
  | Leaf (None, _) -> None
  | Node (l, r) -> (
      match row_context lbl l with
      | Some l -> Some (Node_left (l, r))
      | None ->
          let+ r = row_context lbl r in
          Node_right (l, r))

let let_in p e1 e2 = {expr = Let_in (p, e1, e2)}

let let_in_var x e1 e2 = {expr = Let_in (P_var x, e1, e2)}

let let_in_vector xs e1 e2 = {expr = Let_in (P_vector xs, e1, e2)}

let lets = List.fold_right (fun (lhs, rhs) e -> let_in lhs rhs e)

let lambda rp a b e = {expr = Lambda (rp, a, b, e)}

let lambda_rec rp f a b e = {expr = Lambda_rec (rp, f, a, b, e)}

(*
let size_expr = cata_expr (fun i -> fold_expr_f ( + ) 1 i)

let size_texpr = cata_texpr (fun i _ -> fold_expr_f (fun s (x, _) -> s + x) 1 i)

let substitute subst =
  cata_expr (function
    | Var v -> Option.default (var v) (List.assoc_opt v subst)
    | expr -> {expr})

let tsubstitute subst =
  cata_texpr (function
    | Var v ->
        fun tys -> Option.default {texpr = Var v; tys} (List.assoc_opt v subst)
    | e -> fun tys -> {texpr = map_expr_f fst e; tys})

let mk_variant r lbl x =
  match row_context lbl r with
  | None -> failwith (asprintf "mk_variant %a %s" pp_row r lbl)
  | Some c -> variant (Some lbl) c x

*)

let nil t = prim0 (Nil t)

let some = prim1 Some_

let left a1 a2 t = prim1 (Left (a1, a2, t))

let right a1 a2 t = prim1 (Right (a1, a2, t))

let none t = prim0 (None_ t)

let true_ = lit (Bool true)

let false_ = lit (Bool false)

let dup n = stack_op (Dup n)

let if_ cond l r = {expr = If (cond, l, r)}

let if_none scrutinee l (xr, r) = {expr = If_none (scrutinee, xr, l, r)}

let if_left scrutinee (xl, l) (xr, r) =
  {expr = If_left (scrutinee, xl, l, xr, r)}

let if_cons scrutinee (x, xs, l) r = {expr = If_cons (scrutinee, x, xs, l, r)}

let vector xs = {expr = Vector xs}

let nth i x = {expr = Nth (i, x)}

let pair a1 a2 = prim2 (Pair (a1, a2))

let create_contract c e1 e2 e3 = {expr = Create_contract (c, e1, e2, e3)}

let record_of_tree lbls x = {expr = Record_of_tree (lbls, x)}

let comment c e = {expr = Comment (c, e)}

let map_over xs step input = {expr = Map_over (xs, step, input)}

let iter_over xs step input = {expr = Iter_over (xs, step, input)}

let unpair k x = {expr = Unpair (k, x)}
