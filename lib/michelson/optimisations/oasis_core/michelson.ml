(* Copyright 2022-2023 Morum LLC, 2019-2022 Smart Chain Arena LLC *)
module Big_int = Big_int
module Option = Core.Option
module List = Core.List
include Michelson_base.Primitive
include Michelson_base.Typing
include Michelson_base.Type
open Printf
open Utils.Control

(*List operations*)
let rec map_some f = function
  | [] -> []
  | x :: xs -> (
      match f x with
      | None -> map_some f xs
      | Some y -> y :: map_some f xs)

let somes xs = map_some (fun x -> x) xs

let rec drop i = function
  | _ :: xs when i > 0 -> drop (i - 1) xs
  | xs -> xs

let rec take i = function
| x :: xs when i > 0 -> x :: take (i - 1) xs
| _ -> []

let split_at_opt =
  let open Core in
  let rec split_at acc i = function
    | xs when i = 0 -> Some (List.rev acc, xs)
    | x :: xs when i > 0 -> split_at (x :: acc) (i - 1) xs
    | _ -> None
  in
  fun i l -> split_at [] i l

let split_at ?(err = "split_at") i l =
  match split_at_opt i l with
  | Some l -> l
  | None -> failwith err

let assoc_opt ?(equal = ( = )) key =
  let rec f = function
    | [] -> None
    | (k, v) :: xs -> if equal key k then Some v else f xs
  in
  f

let map2 ?err f xs ys =
  if List.length xs <> List.length ys then
    match err with
    | Some msg -> failwith ("map2: " ^ msg)
    | None -> failwith "map2: lists have different lengths"
  else
    List.map2_exn xs ys ~f

let replicate i x = List.init i (fun _ -> x)

let rec is_prefix eq xs ys =
  match (xs, ys) with
  | [], _ -> true
  | x :: xs, y :: ys -> eq x y && is_prefix eq xs ys
  | _ -> false

let is_suffix eq xs ys = is_prefix eq (List.rev xs) (List.rev ys)
(*End of list operations*)

(* Option operations*)
let option_cata x f = function
  | None -> x
  | Some x -> f x
  
let default d = option_cata d id
(* end of option operations *)

let full_types_and_tags = false

type ad_step =
  | A
  | D
[@@deriving eq, ord, show {with_path = false}]

module Bigint = struct
  type t = Big_int.big_int

  let equal = Big_int.eq_big_int

  let compare = Big_int.compare_big_int

  let show = Big_int.string_of_big_int

  let pp ppf p = Format.fprintf ppf "%s" (show p)

  let of_int = Big_int.big_int_of_int

  let of_string ?msg x =
    match Big_int.big_int_of_string_opt x with
    | None -> failwith ("Bigint.of_string" ^ option_cata "" (( ^ ) ": ") msg)
    | Some x -> x
end

type tezos_int = Bigint.t [@@deriving eq, ord, show {with_path = false}]

type stack =
  | Stack_ok of mtype list
  | Stack_failed
[@@deriving eq, ord, show {with_path = false}]

type 'instr view = {
    name : string
  ; pure : bool
  ; doc : string
  ; tparameter : mtype option
  ; treturn : mtype
  ; onchain_code : 'instr option
  ; offchain_code : 'instr
}
[@@deriving eq, ord, show {with_path = false}, map, fold]

type ('i, 'literal) instr_f =
  | MI0 of mtype prim0
  | MI1 of mtype prim1
  | MI1_fail of prim1_fail
  | MI2 of mtype prim2
  | MI3 of prim3
  | MIerror of string
  | MIcomment of string list
  | MImich of {
        name : string
      ; parsed : Micheline.t
      ; typesIn : mtype list
      ; typesOut : mtype list
    }
  | MIdip of 'i
  | MIdipn of int * 'i
  | MIloop of 'i
  | MIloop_left of 'i
  | MIiter of 'i
  | MImap of 'i
  | MIdrop
  | MIdropn of int
  | MIdup of int
  | MIdig of int
  | MIdug of int
  | MIif of 'i * 'i
  | MIif_left of 'i * 'i
  | MIif_none of 'i * 'i
  | MIif_cons of 'i * 'i
  | MIpush of mtype * 'literal
  | MIseq of 'i list
  | MIswap
  | MIunpair of bool list
  | MIpairn of int
  | MIfield of ad_step list
  | MIsetField of ad_step list
  | MIlambda of mtype * mtype * 'i
  | MIlambda_rec of mtype * mtype * 'i
  | MIcreate_contract of {
        tparameter : mtype * string option
      ; tstorage : mtype
      ; code : 'i
      ; views : 'i view list
    }
  | MIconcat1
  | MIconcat2
  | MIconcat_unresolved
  | MIConstant of 'literal
[@@deriving eq, ord, show {with_path = false}, map, fold]

type ('instr, 'literal) literal_f =
  | Int of tezos_int
  | Bool of bool
  | String of string
  | Bytes of string
  | Unit
  | Pair of 'literal * 'literal
  | None_
  | Left of 'literal
  | Right of 'literal
  | Some_ of 'literal
  | Seq of 'literal list
  | Elt of ('literal * 'literal)
  | Instr of 'instr
  | Lambda_rec of 'instr
  | AnyMap of ('literal * 'literal) list
  | Constant of string
[@@deriving eq, ord, show {with_path = false}, map, fold]

let sequence_literal_f =
  let open Result in
  let open Utils.Control in
  function
  | (Int _ | Bool _ | String _ | Bytes _ | Unit | None_ | Constant _) as l ->
      Ok l
  | Pair (x, y) -> map2 (fun x y -> Pair (x, y)) x y
  | Left x -> map (fun x -> Left x) x
  | Right x -> map (fun x -> Right x) x
  | Some_ x -> map (fun x -> Some_ x) x
  | Seq xs -> map (fun x -> Seq x) (sequence_list xs)
  | Elt (x, y) -> map2 (fun x y -> Elt (x, y)) x y
  | AnyMap xs -> map (fun x -> AnyMap x) (map_list (uncurry (map2 pair)) xs)
  | Instr x -> map (fun x -> Instr x) x
  | Lambda_rec x -> map (fun x -> Lambda_rec x) x

let sequence_view ({onchain_code; offchain_code} as view) =
  let open Result in
  let+ onchain_code = sequence_option onchain_code
  and+ offchain_code = offchain_code in
  {view with onchain_code; offchain_code}

let sequence_instr_f =
  let open Result in
  function
  | MIdip x -> map (fun x -> MIdip x) x
  | MIdipn (n, x) -> map (fun x -> MIdipn (n, x)) x
  | MIloop x -> map (fun x -> MIloop x) x
  | MIloop_left x -> map (fun x -> MIloop_left x) x
  | MIiter x -> map (fun x -> MIiter x) x
  | MImap x -> map (fun x -> MImap x) x
  | MIif (i1, i2) -> map2 (fun i1 i2 -> MIif (i1, i2)) i1 i2
  | MIif_left (i1, i2) -> map2 (fun i1 i2 -> MIif_left (i1, i2)) i1 i2
  | MIif_none (i1, i2) -> map2 (fun i1 i2 -> MIif_none (i1, i2)) i1 i2
  | MIif_cons (i1, i2) -> map2 (fun i1 i2 -> MIif_cons (i1, i2)) i1 i2
  | MIlambda (t1, t2, i) -> map (fun x -> MIlambda (t1, t2, x)) i
  | MIlambda_rec (t1, t2, i) -> map (fun x -> MIlambda_rec (t1, t2, x)) i
  | MIcreate_contract {tparameter; tstorage; code; views} ->
      let+ code = code
      and+ views = map_list sequence_view views in
      MIcreate_contract {tparameter; tstorage; code; views}
  | MIseq is -> map (fun is -> MIseq is) (sequence_list is)
  | MIpush (t, l) -> map (fun l -> MIpush (t, l)) l
  | MIConstant l -> map (fun l -> MIConstant l) l
  | ( MI0 _ | MI1 _ | MI1_fail _ | MI2 _
    | MI3
        ( Slice
        | Update
        | Get_and_update
        | Transfer_tokens
        | Check_signature
        | Open_chest )
    | MIdrop
    | MIswap
    | MIerror _
    | MIcomment _
    | MImich _
    | MIdropn _
    | MIdup _
    | MIdig _
    | MIdug _
    | MIunpair _
    | MIpairn _
    | MIfield _
    | MIsetField _
    | MIconcat1
    | MIconcat2
    | MIconcat_unresolved) as instr -> return instr

type instr = {instr : (instr, literal) instr_f}

and literal = {literal : (instr, literal) literal_f}
[@@deriving eq, ord, show {with_path = false}, map, fold]

type ('i, 'l) alg = {
    f_instr : ('i, 'l) instr_f -> 'i
  ; f_literal : ('i, 'l) literal_f -> 'l
}

let cata {f_instr; f_literal} =
  let rec cata_instr {instr} =
    f_instr (map_instr_f cata_instr cata_literal instr)
  and cata_literal {literal} =
    f_literal (map_literal_f cata_instr cata_literal literal)
  in
  (cata_instr, cata_literal)

let cata_instr alg = fst (cata alg)

let cata_literal alg = snd (cata alg)

module MLiteral = struct
  let compare x y = compare_literal x y

  let int i = {literal = Int i}

  let small_int i = {literal = Int (Bigint.of_int i)}

  let bool x = {literal = Bool x}

  let string x = {literal = String x}

  let bytes x = {literal = Bytes x}

  let unit = {literal = Unit}

  let left x = {literal = Left x}

  let right x = {literal = Right x}

  let some x = {literal = Some_ x}

  let pair x1 x2 = {literal = Pair (x1, x2)}

  let none = {literal = None_}

  let list xs = {literal = Seq xs}

  let set xs = {literal = Seq (Base.List.dedup_and_sort ~compare xs)}

  let seq xs = {literal = Seq xs}

  let elt k v = {literal = Elt (k, v)}

  let mk_map xs =
    {
      literal =
        AnyMap
          (Base.List.dedup_and_sort
             ~compare:(fun (k1, _) (k2, _) -> compare k1 k2)
             xs)
    }

  let sapling_empty_state = seq []

  let constant hash = {literal = Constant hash}

  let instr body = {literal = Instr body}

  let lambda_rec body = {literal = Lambda_rec body}
end

let string_of_ad_path p =
  let f = function
    | A -> "A"
    | D -> "D"
  in
  String.concat "" (List.map ~f:f p)

let strip_annots {mt} = mk_mtype mt

let strip_annot_variable {mt; annot_type} = mk_mtype mt ?annot_type

(** {1 Stack helpers} *)

type tliteral = {
    tliteral : (tinstr, tliteral) literal_f
  ; t : mtype Result.t
}

and tinstr = {
    tinstr : (tinstr, tliteral) instr_f
  ; stack_in : stack Result.t
  ; stack_out : stack Result.t
}
[@@deriving eq, ord, show {with_path = false}]

type ('i, 'l) talg = {
    f_tinstr :
         stack_in:stack Result.t
      -> stack_out:stack Result.t
      -> ('i, 'l) instr_f
      -> 'i
  ; f_tliteral : t:mtype Result.t -> ('i, 'l) literal_f -> 'l
}

let tcata {f_tinstr; f_tliteral} =
  let rec cata_tinstr {tinstr; stack_in; stack_out} =
    f_tinstr ~stack_in ~stack_out (map_instr_f cata_tinstr cata_tliteral tinstr)
  and cata_tliteral {tliteral; t} =
    f_tliteral ~t (map_literal_f cata_tinstr cata_tliteral tliteral)
  in
  (cata_tinstr, cata_tliteral)

let cata_tinstr alg = fst (tcata alg)

let cata_tliteral alg = snd (tcata alg)

type instr_spec = {
    name : string
  ; rule :
         tparameter:mtype * string option
      -> mtype list
      -> (stack Result.t -> tinstr, mtype -> tliteral) instr_f
      -> (tinstr, tliteral) instr_f * stack Result.t
  ; commutative : bool
  ; arities : (int * int) option
}

let mk_spec_raw name ?commutative ?arities rule =
  let commutative = Option.is_some commutative in
  { name; commutative; arities; rule }

let mk_spec name ?commutative ?arities rule =
  let rule ~tparameter stack instr =
    let err msg =
      let tinstr =
        map_instr_f
          (fun x -> x (Error (name ^ " error")))
          (fun _ -> assert false)
          instr
      in
      (tinstr, Error msg)
    in
    match rule ~tparameter stack instr with
    | Some x -> x
    | None -> err (name ^ ": unexpected stack")
  in
  let commutative = Option.is_some commutative in
  {name; commutative; arities; rule}

let mk_spec_no_sub name ?commutative ?arities rule =
  let rule ~tparameter stack instr =
    let tinstr =
      map_instr_f (fun _ -> assert false) (fun _ -> assert false) instr
    in
    let stack = rule ~tparameter stack in
    Some (tinstr, stack)
  in
  mk_spec name ?commutative ?arities rule

let mk_spec_basic name ?commutative ~arities:(a_in, a_out) rule =
  let rule ~tparameter:_ stack instr =
    match rule stack with
    | None -> None
    | Some xs ->
        assert (List.length xs = a_out);
        let tinstr =
          map_instr_f (fun _ -> assert false) (fun _ -> assert false) instr
        in
        let stack = Ok (Stack_ok (xs @ drop a_in stack)) in
        Some (tinstr, stack)
  in
  mk_spec name ?commutative ~arities:(a_in, a_out) rule

let mk_spec_const name t =
  mk_spec_basic ~arities:(0, 1) name (fun _ -> Some [t])

(** {1 Unification}  *)

let unifiable_types t u = Result.is_ok (unify_types ~tolerant:() t u)

let unify_stack_elements t1 t2 =
  match unify_types ~tolerant:() t1 t2 with
  | Ok t -> Some t
  | Error _ -> None

let rec unify_ok_stacks s1 s2 =
  match (s1, s2) with
  | se1 :: s1, se2 :: s2 ->
      Option.map2
        ~f:(fun x xs -> x :: xs)
        (unify_stack_elements se1 se2)
        (unify_ok_stacks s1 s2)
  | [], [] -> Some []
  | _ -> None

let unifiable_ok_stacks t u = Option.is_some (unify_ok_stacks t u)

let unify_stacks s1 s2 =
  match (s1, s2) with
  | Stack_ok s1, Stack_ok s2 ->
      Option.map ~f:(fun x -> Stack_ok x) (unify_ok_stacks s1 s2)
  | Stack_failed, s2 -> Some s2
  | s1, Stack_failed -> Some s1

let initial_stack ~tparameter ~tstorage =
  Stack_ok
    [
      mt_pair
        {tparameter with annot_variable = Some "parameter"}
        {tstorage with annot_variable = Some "storage"}
    ]

(** {1 Michelson instructions} *)

let mi_seq =
  let rule ~tparameter:_ stack = function
    | MIseq xs ->
        let rec f r stack = function
          | [] -> Some (MIseq (List.rev r), stack)
          | x :: xs ->
              let (x : tinstr) = x stack in
              f (x :: r) x.stack_out xs
        in
        f [] (Ok (Stack_ok stack)) xs
    | _ -> assert false
  in
  mk_spec "(instruction sequence)" rule

let mi_comment =
  let rule ~tparameter:_ stack = Ok (Stack_ok stack) in
  mk_spec_no_sub "(comment instruction)" rule

let mi_error s =
  let rule ~tparameter:_ _stack = Error s in
  mk_spec_no_sub "(error instruction)" rule

let mi_failwith =
  let rule ~tparameter:_ = function
    | _ :: _ -> Ok Stack_failed
    | [] -> Error "FAILWITH on empty stack"
  in
  mk_spec_no_sub "FAILWITH" ~arities:(1, 0) rule

let mi_never =
  let rule ~tparameter:_ = function
    | {mt = MT0 Never} :: _ -> Ok Stack_failed
    | _ -> Error "NEVER on empty stack"
  in
  mk_spec_no_sub "NEVER" ~arities:(1, 0) rule

let mi_ticket =
  mk_spec_basic "TICKET" ~arities:(2, 1) (function
    | t :: {mt = MT0 Nat} :: _ -> Some [mt_option (mt_ticket t)]
    | _ -> None)

let mi_ticket_deprecated =
  mk_spec_basic "TICKET_DEPRECATED" ~arities:(2, 1) (function
    | t :: {mt = MT0 Nat} :: _ -> Some [mt_ticket t]
    | _ -> None)

let mi_read_ticket =
  mk_spec_basic "READ_TICKET" ~arities:(1, 2) (function
    | {mt = MT1 (Ticket, t)} :: _ ->
        Some [mt_pair mt_address (mt_pair t mt_nat); mt_ticket t]
    | _ -> None)

let mi_join_tickets =
  mk_spec_basic "JOIN_TICKETS" ~arities:(1, 1) (function
    | {
        mt =
          MT2
            ( Pair _
            , ({mt = MT1 (Ticket, _)} as t1)
            , ({mt = MT1 (Ticket, _)} as t2) )
      }
      :: _
      when unifiable_types t1 t2 -> Some [mt_option t1]
    | _ -> None)

let mi_split_ticket =
  mk_spec_basic "SPLIT_TICKET" ~arities:(2, 1) (function
    | ({mt = MT1 (Ticket, _)} as t)
      :: {mt = MT2 (Pair _, {mt = MT0 Nat}, {mt = MT0 Nat})}
      :: _ -> Some [mt_option (mt_pair t t)]
    | _ -> None)

let mi_pairing_check =
  mk_spec_basic "PAIRING_CHECK" ~arities:(1, 1) (function
    | {
        mt =
          MT1
            ( List
            , {
                mt =
                  MT2 (Pair _, {mt = MT0 Bls12_381_g1}, {mt = MT0 Bls12_381_g2})
              } )
      }
      :: _ -> Some [mt_bool]
    | _ -> None)

let cond_aux x y =
  let open Result in
  let* sx = x.stack_out in
  let* sy = y.stack_out in
  option_cata (error "cannot unify branches") ok (unify_stacks sx sy)

let mi_if =
  let rule ~tparameter:_ stack instr =
    match (stack, instr) with
    | {mt = MT0 Bool} :: tail, MIif (x, y) ->
        let x = x (Ok (Stack_ok tail)) in
        let y = y (Ok (Stack_ok tail)) in
        Some (MIif (x, y), cond_aux x y)
    | _, MIif _ -> None
    | _ -> assert false
  in
  mk_spec "IF" rule

let mi_if_none =
  let rule ~tparameter:_ stack instr =
    match (stack, instr) with
    | {mt = MT1 (Option, t)} :: tail, MIif_none (x, y) ->
        let a =
          match t.annot_variable with
          | Some v -> v ^ ".some"
          | None -> "some"
        in
        let t = {t with annot_variable = Some a} in
        let x = x (Ok (Stack_ok tail)) in
        let y = y (Ok (Stack_ok (t :: tail))) in
        Some (MIif_none (x, y), cond_aux x y)
    | _, MIif_none _ -> None
    | _ -> assert false
  in
  mk_spec "IF_NONE" rule

let mi_if_left =
  let rule ~tparameter:_ stack instr =
    match (stack, instr) with
    | ( {mt = MT2 (Or {annot_left; annot_right}, t, u); annot_variable} :: tail
      , MIif_left (x, y) ) ->
        let open Option in
        let fa v = v ^ "." ^ default "left" annot_left in
        let t = {t with annot_variable = Option.map ~f:fa annot_variable} in
        let fa v = v ^ "." ^ default "right" annot_right in
        let u = {u with annot_variable = Option.map ~f:fa annot_variable} in
        let x = x (Ok (Stack_ok (t :: tail))) in
        let y = y (Ok (Stack_ok (u :: tail))) in
        return (MIif_left (x, y), cond_aux x y)
    | _, MIif_left _ -> None
    | _ -> assert false
  in
  mk_spec "IF_LEFT" rule

let mi_if_cons =
  let rule ~tparameter:_ stack instr =
    match (stack, instr) with
    | {mt = MT1 (List, t)} :: tail, MIif_cons (x, y) ->
        let open Option in
        let x = x (Ok (Stack_ok (t :: mt_list t :: tail))) in
        let y = y (Ok (Stack_ok tail)) in
        return (MIif_cons (x, y), cond_aux x y)
    | _, MIif_cons _ -> None
    | _ -> assert false
  in
  mk_spec "IF_CONS" rule

let mi_dip =
  let rule ~tparameter:_ stack = function
    | MIdip body -> (
        match stack with
        | [] -> None
        | t :: tail -> (
            let body = body (Ok (Stack_ok tail)) in
            let tinstr = MIdip body in
            match body.stack_out with
            | Ok (Stack_ok tail') -> Some (tinstr, Ok (Stack_ok (t :: tail')))
            | _ -> Some (tinstr, Error "DIP: body error")))
    | _ -> assert false
  in
  mk_spec "DIP" rule

let mi_dipn =
  let rule ~tparameter:_ stack = function
    | MIdipn (n, body) when n <= List.length stack -> (
        assert (n >= 0);
        let body = body (Ok (Stack_ok (drop n stack))) in
        let tinstr = MIdipn (n, body) in
        match body.stack_out with
        | Ok (Stack_ok stack') ->
            Some (tinstr, Ok (Stack_ok (take n stack @ stack')))
        | _ -> Some (tinstr, Error "DIP: body error"))
    | _ -> assert false
  in
  mk_spec "DIPn" rule

let is_hot =
  let is_hot_function ?annot_type:_ ?annot_variable:_ mt =
    match mt with
    | MT1 (Ticket, _) -> true
    | MT1 (Contract, _) | MT2 (Lambda, _, _) -> false
    | MT_var _ -> false  (* Actually maybe, but we care about strong yes*)
    | t -> fold_mtype_f (||) false t
  in
  cata_mtype is_hot_function

let is_duppable t = not (is_hot t) 

let mi_dup i =
  assert (i >= 1);
  let rec get acc n = function
    | x :: _ when n = 1 ->
        if is_duppable x then Some (x :: List.rev (x :: acc)) else None
    | x :: rest -> get (x :: acc) (n - 1) rest
    | [] -> None
  in
  mk_spec_basic "DUP" ~arities:(i, i + 1) (get [] i)

let mi_dig n =
  let rule ~tparameter:_ stack =
    match split_at_opt n stack with
    | None -> Error (sprintf "DIG %i: stack too short" n)
    | Some (hi, lo) -> (
        match lo with
        | [] -> Error (sprintf "DIG %i: stack too short" n)
        | x :: lo -> Ok (Stack_ok ((x :: hi) @ lo)))
  in
  mk_spec_no_sub "DIG" rule

let mi_dug n =
  let rule ~tparameter:_ = function
    | x :: tail ->
        if n > List.length tail
        then Error (sprintf "DUG %i: stack too short" n)
        else
          let hi, lo = split_at n tail in
          Ok (Stack_ok (hi @ (x :: lo)))
    | [] -> Error "DUG: empty stack"
  in
  mk_spec_no_sub "DUG" rule

let mi_swap =
  mk_spec_basic "SWAP" ~arities:(2, 2) (function
    | a :: b :: _ -> Some [b; a]
    | _ -> None)

let mi_drop =
  mk_spec_basic "DROP" ~arities:(1, 0) (function
    | _ :: _ -> Some []
    | [] -> None)

let mi_dropn n =
  mk_spec_basic "DROP" ~arities:(n, 0) (function
    | _ :: _ -> Some []
    | [] -> None)

let unpair_size = List.fold_left ~f:(fun acc x -> acc + if x then 1 else 0) ~init:0

let unpair_arg xs =
  let open Core in
  if List.for_all ~f:Fn.id xs
  then Int.to_string (List.length xs)
  else
    let bools_as_strings = List.map ~f:Bool.to_string xs in
    sprintf "[%s]" (String.concat ~sep:"; " bools_as_strings)

let mi_unpair fields =
  assert (List.length fields >= 2);
  let rec aux acc fields stack =
    match (stack, fields) with
    | _, [] -> Some (List.rev acc)
    | _ :: _, [false] -> Some (List.rev acc)
    | se :: _, [true] -> Some (List.rev (se :: acc))
    | {mt = MT2 (Pair _, fst, snd)} :: rest, select :: fields ->
        let acc = if select then fst :: acc else acc in
        aux acc fields (snd :: rest)
    | _ -> None
  in
  mk_spec_basic "UNPAIR" ~arities:(1, unpair_size fields) (aux [] fields)

let mi_pairn n =
  let rec aux acc n stack =
    if n = 0
    then
      let rec fold acc = function
        | x :: rest -> fold (mt_pair x acc) rest
        | [] -> acc
      in
      match acc with
      | [] -> None
      | x :: rest -> Some [fold x rest]
    else
      match stack with
      | se :: rest -> aux (se :: acc) (n - 1) rest
      | _ -> None
  in
  mk_spec_basic "PAIR" ~arities:(n, 1) (aux [] n)

let mi_iter =
  let rule ~tparameter:_ stack instr =
    match (stack, instr) with
    | c :: tail, MIiter body -> (
        let el =
          match c.mt with
          | MT2 (Map, k, v) -> Some (mt_pair k v)
          | MT1 ((List | Set), t) -> Some t
          | _ -> None
        in
        match el with
        | None -> None
        | Some el ->
            let body = body (Ok (Stack_ok (el :: tail))) in
            let tinstr = MIiter body in
            let ok =
              match body.stack_out with
              | Ok (Stack_ok stack') when unifiable_ok_stacks tail stack' ->
                  true
              | Ok Stack_failed -> true
              | _ -> false
            in
            let s =
              if ok then Ok (Stack_ok tail) else Error "ITER: incompatible body"
            in
            Some (tinstr, s))
    | [], MIiter _ -> None
    | _ -> assert false
  in
  mk_spec "ITER" rule

let mi_loop =
  let rule ~tparameter:_ stack = function
    | MIloop body -> (
        match stack with
        | {mt = MT0 Bool} :: tail -> (
            let body = body (Ok (Stack_ok tail)) in
            let tinstr = MIloop body in
            match body.stack_out with
            | Ok (Stack_ok ({mt = MT0 Bool} :: tail'))
              when unifiable_ok_stacks tail tail' ->
                Some (tinstr, Ok (Stack_ok tail))
            | _ -> Some (tinstr, Error "LOOP: incompatible body"))
        | _ -> None)
    | _ -> assert false
  in
  mk_spec "LOOP" rule

let mi_loop_left =
  let rule ~tparameter:_ stack = function
    | MIloop_left body -> (
        match stack with
        | {mt = MT2 (Or _, a, b)} :: tail -> (
            let body = body (Ok (Stack_ok (a :: tail))) in
            let tinstr = MIloop_left body in
            match body.stack_out with
            | Ok (Stack_ok ({mt = MT2 (Or _, a', b')} :: tail'))
              when unifiable_types a a' && unifiable_types b b'
                   && unifiable_ok_stacks tail tail' ->
                Some (tinstr, Ok (Stack_ok (b :: tail)))
            | _ -> 
              Some (tinstr, Error "LOOP_LEFT: incompatible body"))
        | _ -> None)
    | _ -> assert false
  in
  mk_spec "LOOP_LEFT" rule

let mi_lambda =
  let rule ~tparameter:_ stack = function
    | MIlambda (t_in, t_out, body) ->
        let body = body (Ok (Stack_ok [t_in])) in
        let tinstr = MIlambda (t_in, t_out, body) in
        let stack =
          let ok = Ok (Stack_ok (mt_lambda t_in t_out :: stack)) in
          match body.stack_out with
          | Ok (Stack_ok [t_out']) when unifiable_types t_out t_out' -> ok
          | Ok (Stack_ok _) -> Error "LAMBDA: non-singleton result stack"
          | Ok Stack_failed -> ok
          | Error s -> Error (Printf.sprintf "lambda stack error %s" s)
        in
        (tinstr, stack)
    | _ -> assert false
  in
  mk_spec_raw "LAMBDA" rule

let mi_lambda_rec =
  let rule ~tparameter:_ stack = function
    | MIlambda_rec (t_in, t_out, body) ->
        let body = body (Ok (Stack_ok [t_in; mt_lambda t_in t_out])) in
        let tinstr = MIlambda_rec (t_in, t_out, body) in
        let stack =
          let ok = Ok (Stack_ok (mt_lambda t_in t_out :: stack)) in
          match body.stack_out with
          | Ok (Stack_ok [t_out']) when unifiable_types t_out t_out' -> ok
          | Ok (Stack_ok _) -> Error "LAMBDA_REC: non-singleton result stack"
          | Ok Stack_failed -> ok
          | Error s -> Error (Printf.sprintf "lambda_rec stack error %s" s)
        in
        (tinstr, stack)
    | _ -> assert false
  in
  mk_spec_raw "LAMBDA_REC" rule

let mi_map =
  let rule ~tparameter:_ stack instr =
    match (stack, instr) with
    | c :: tail, MImap body -> (
        let wrap_v =
          match c.mt with
          | MT2 (Map, k, v) -> Some (mt_map k, mt_pair k v)
          | MT1 (List, v) -> Some (mt_list, v)
          | MT1 (Option, v) -> Some (mt_option, v)
          | _ -> None
        in
        match wrap_v with
        | None -> None
        | Some (wrap, v) -> (
            let body = body (Ok (Stack_ok (v :: tail))) in
            let tinstr = MImap body in
            match body.stack_out with
            | Ok (Stack_ok (v' :: tail')) when unifiable_ok_stacks tail tail' ->
                Some (tinstr, Ok (Stack_ok (wrap v' :: tail)))
            | _ -> Some (tinstr, Error "MAP: incompatible body")))
    | [], MImap _ -> None
    | _ -> assert false
  in
  mk_spec "MAP" rule

let mi_pair ?annot_fst ?annot_snd () =
  mk_spec_basic "PAIR" ~arities:(2, 1) (function
    | a :: b :: _ -> Some [mt_pair ?annot_fst ?annot_snd a b]
    | _ -> None)

let mi_cons =
  mk_spec_basic "CONS" ~arities:(2, 1) (function
    | a :: {mt = MT1 (List, a')} :: _ -> (
        match unify_types ~tolerant:() a a' with
        | Ok t -> Some [mt_list (strip_annots t)]
        | Error _ -> None)
    | _ -> None)

let mi_get =
  mk_spec_basic "GET" ~arities:(2, 1) (function
    | key :: {mt = MT2 (Map, key', value)} :: _ when unifiable_types key key' ->
        Some [mt_option value]
    | key :: {mt = MT2 (Big_map, key', value)} :: _
      when unifiable_types key key' -> Some [mt_option value]
    | _ -> None)

let rec get_comb_type n = function
  | t when n = 0 -> Some t
  | {mt = MT2 (Pair _, fst, _)} when n = 1 -> Some fst
  | {mt = MT2 (Pair _, _, snd)} -> get_comb_type (n - 2) snd
  | _ -> None

let mi_getn n =
  mk_spec_basic "GET" ~arities:(1, 1) (function
    | t :: _ -> Option.map ~f:(fun t -> [t]) (get_comb_type n t)
    | [] -> None)

let mi_updaten n =
  mk_spec_basic "UPDATE" ~arities:(2, 1) (function
    | t1 :: t :: _ -> (
        match get_comb_type n t with
        | None -> None
        | Some t2 -> if unifiable_types t1 t2 then Some [t] else None)
    | _ :: _ | [] -> None)

let mi_eq =
  mk_spec_basic "EQ" ~commutative:() ~arities:(1, 1) (function
    | {mt = MT0 Int} :: _ -> Some [mt_bool]
    | _ -> None)

let mi_neq = {mi_eq with name = "NEQ"}

let mi_lt = {mi_neq with name = "LT"; commutative = false}

let mi_le = {mi_lt with name = "LE"}

let mi_gt = {mi_lt with name = "GT"}

let mi_ge = {mi_lt with name = "GE"}

let mi_neg =
  mk_spec_basic "NEG" ~arities:(1, 1) (function
    | {mt = MT0 Nat} :: _ -> Some [mt_int]
    | {mt = MT0 Int} :: _ -> Some [mt_int]
    | {mt = MT0 Bls12_381_g1} :: _ -> Some [mt_bls12_381_g1]
    | {mt = MT0 Bls12_381_g2} :: _ -> Some [mt_bls12_381_g2]
    | {mt = MT0 Bls12_381_fr} :: _ -> Some [mt_bls12_381_fr]
    | _ -> None)

let mi_int =
  mk_spec_basic "INT" ~arities:(1, 1) (function
    | {mt = MT0 Bytes} :: _ -> Some [mt_int]
    | {mt = MT0 Nat} :: _ -> Some [mt_int]
    | {mt = MT0 Bls12_381_fr} :: _ -> Some [mt_int]
    | _ -> None)

let mi_nat =
  mk_spec_basic "NAT" ~arities:(1, 1) (function
    | {mt = MT0 Bytes} :: _ -> Some [mt_nat]
    | _ -> None)

let mi_bytes =
  mk_spec_basic "BYTES" ~arities:(1, 1) (function
    | {mt = MT0 Int} :: _ -> Some [mt_bytes]
    | {mt = MT0 Nat} :: _ -> Some [mt_bytes]
    | _ -> None)

let rec is_comparable mtype =
  match mtype.mt with
  | MT0
      ( Unit
      | Bool
      | Nat
      | Int
      | Mutez
      | String
      | Bytes
      | Chain_id
      | Timestamp
      | Address
      | Key
      | Key_hash
      | Signature
      | Never ) -> true
  | MT1 (Option, t) -> is_comparable t
  | MT2 (Pair _, t1, t2) | MT2 (Or _, t1, t2) ->
      is_comparable t1 && is_comparable t2
  | MT0
      ( Operation
      | Sapling_state _
      | Sapling_transaction _
      | Bls12_381_g1
      | Bls12_381_g2
      | Bls12_381_fr
      | Chest_key
      | Chest )
  | MT1 ((List | Set | Contract | Ticket), _)
  | MT2 ((Lambda | Map | Big_map), _, _)
  | MT_var _ -> false

let mi_compare =
  mk_spec_basic "COMPARE" ~arities:(2, 1) (function
    | a :: b :: _ when is_comparable a && is_comparable b && unifiable_types a b
      -> Some [mt_int]
    | _ -> None)

let mi_sub =
  mk_spec_basic "SUB" ~arities:(2, 1) (function
    | {mt = MT0 Int} :: {mt = MT0 Int} :: _ -> Some [mt_int]
    | {mt = MT0 Int} :: {mt = MT0 Nat} :: _ -> Some [mt_int]
    | {mt = MT0 Nat} :: {mt = MT0 Int} :: _ -> Some [mt_int]
    | {mt = MT0 Nat} :: {mt = MT0 Nat} :: _ -> Some [mt_int]
    | {mt = MT0 Mutez} :: {mt = MT0 Mutez} :: _ -> Some [mt_mutez]
    | {mt = MT0 Timestamp} :: {mt = MT0 Int} :: _ -> Some [mt_timestamp]
    | {mt = MT0 Timestamp} :: {mt = MT0 Timestamp} :: _ -> Some [mt_int]
    | _ -> None)

let mi_ediv =
  mk_spec_basic "EDIV" ~arities:(2, 1) (function
    | {mt = MT0 Int} :: {mt = MT0 Int} :: _ ->
        Some [mt_option (mt_pair mt_int mt_nat)]
    | {mt = MT0 Int} :: {mt = MT0 Nat} :: _ ->
        Some [mt_option (mt_pair mt_int mt_nat)]
    | {mt = MT0 Nat} :: {mt = MT0 Int} :: _ ->
        Some [mt_option (mt_pair mt_int mt_nat)]
    | {mt = MT0 Nat} :: {mt = MT0 Nat} :: _ ->
        Some [mt_option (mt_pair mt_nat mt_nat)]
    | {mt = MT0 Mutez} :: {mt = MT0 Nat} :: _ ->
        Some [mt_option (mt_pair mt_mutez mt_mutez)]
    | {mt = MT0 Mutez} :: {mt = MT0 Mutez} :: _ ->
        Some [mt_option (mt_pair mt_nat mt_mutez)]
    | _ -> None)

let mi_not :_ =
  mk_spec_basic "NOT" ~commutative:() ~arities:(1, 1) (function
    | {mt = MT0 Bool} :: _ -> Some [mt_bool]
    | {mt = MT0 Nat} :: _ -> Some [mt_int]
    | {mt = MT0 Int} :: _ -> Some [mt_int]
    | {mt = MT0 Bytes} :: _ -> Some [mt_bytes]
    | _ -> None)

let mi_and :_ =
  mk_spec_basic "AND" ~commutative:() ~arities:(2, 1) (function
    | {mt = MT0 Bool} :: {mt = MT0 Bool} :: _ -> Some [mt_bool]
    | {mt = MT0 Nat} :: {mt = MT0 Nat} :: _ -> Some [mt_nat]
    | {mt = MT0 Int} :: {mt = MT0 Nat} :: _ -> Some [mt_nat]
    | {mt = MT0 Bytes} :: {mt = MT0 Bytes} :: _ -> Some [mt_bytes]
    | _ -> None)

let mi_or :_ =
  mk_spec_basic "OR" ~commutative:() ~arities:(2, 1) (function
    | {mt = MT0 Bool} :: {mt = MT0 Bool} :: _ -> Some [mt_bool]
    | {mt = MT0 Nat} :: {mt = MT0 Nat} :: _ -> Some [mt_nat]
    | {mt = MT0 Bytes} :: {mt = MT0 Bytes} :: _ -> Some [mt_bytes]
    | _ -> None)

let mi_xor  = {(mi_or ) with name = "XOR"}

let mi_shift_left :_ =
  mk_spec_basic "LSL" ~arities:(2, 1) (function
    | {mt = MT0 Nat} :: {mt = MT0 Nat} :: _ -> Some [mt_nat]
    | {mt = MT0 Bytes} :: {mt = MT0 Nat} :: _ -> Some [mt_bytes]
    | _ -> None)

let mi_shift_right  = {(mi_shift_left ) with name = "LSR"}

let mi_unit = mk_spec_const "UNIT" mt_unit

let mi_nil t = mk_spec_const "NIL" (mt_list t)

let mi_empty_set t = mk_spec_const "EMPTY_SET" (mt_set t)

let mi_empty_map k v = mk_spec_const "EMPTY_MAP" (mt_map k v)

let mi_empty_big_map k v = mk_spec_const "EMPTY_BIG_MAP" (mt_big_map k v)

let mi_none t = mk_spec_const "NONE" (mt_option t)

let is_pushable t =
  match t.mt with
  | MT2 (Big_map, _, _) -> false
  | MT1 (Contract, _) -> false
  | MT0 Operation -> false
  | MT0 (Sapling_state _) -> false
  | MT1 (Ticket, _) -> false
  | MT0 Unit
  | MT0 Bool
  | MT0 Nat
  | MT0 Int
  | MT0 Mutez
  | MT0 String
  | MT0 Bytes
  | MT0 Chain_id
  | MT0 Timestamp
  | MT0 Address
  | MT0 Key
  | MT0 Key_hash
  | MT0 Signature
  | MT0 (Sapling_transaction _)
  | MT0 Never
  | MT0 Bls12_381_g1
  | MT0 Bls12_381_g2
  | MT0 Bls12_381_fr
  | MT0 Chest_key
  | MT0 Chest
  | MT1 (Option, _)
  | MT1 (List, _)
  | MT1 (Set, _)
  | MT2 (Lambda, _, _)
  | MT2 (Map, _, _)
  | MT2 (Pair _, _, _)
  | MT2 (Or _, _, _) -> true
  | MT_var _ -> false

let mi_push =
  let rule ~tparameter:_ stack = function
    | MIpush (t, l) ->
        let l = l t in
        let tinstr = MIpush (t, l) in
        let stack =
          if is_pushable t
          then
            match l.t with
            | Ok _ -> Ok (Stack_ok (t :: stack))
            | Error e -> Error e
          else Error "Type is not pushable"
        in
        (tinstr, stack)
    | _ -> assert false
  in
  mk_spec_raw "PUSH" ~arities:(0, 1) rule

let mi_some =
  mk_spec_basic "SOME" ~arities:(1, 1) (function
    | t :: _ -> Some [mt_option (strip_annots t)]
    | [] -> None)

let mi_left ?annot_left ?annot_right b =
  mk_spec_basic "LEFT" ~arities:(1, 1) (function
    | a :: _ -> Some [mt_or ?annot_left ?annot_right (strip_annots a) b]
    | [] -> None)

let mi_right ?annot_left ?annot_right a =
  mk_spec_basic "RIGHT" ~arities:(1, 1) (function
    | b :: _ -> Some [mt_or ?annot_left ?annot_right a (strip_annots b)]
    | [] -> None)

(** Select the part of the type designated by the ad_path. *)
let rec ad_path_in_type ops t =
  match (ops, t) with
  | [], _ -> Some t
  | A :: p, {mt = MT2 (Pair _, fst, _)} -> ad_path_in_type p fst
  | D :: p, {mt = MT2 (Pair _, _, snd)} -> ad_path_in_type p snd
  | _ :: _, _ -> None

let mi_field steps =
  mk_spec_basic ~arities:(1, 1)
    (sprintf "C%sR" (string_of_ad_path steps))
    (function
      | t :: _ -> (
          match ad_path_in_type steps t with
          | None -> None
          | Some t -> Some [t])
      | [] -> None)

let mi_set_field steps =
  mk_spec_basic ~arities:(2, 1)
    (sprintf "SET_C%sR" (string_of_ad_path steps))
    (function
      | t :: x :: _ -> (
          match ad_path_in_type steps t with
          | Some x' when unifiable_types x x' -> Some [t]
          | _ -> None)
      | _ -> None)

let mi_update =
  mk_spec_basic "UPDATE" ~arities:(3, 1) (function
    | k :: {mt = MT0 Bool} :: {mt = MT1 (Set, k')} :: _ ->
        Option.map
          ~f:(fun k -> [mt_set k])
          (Result.get_ok (unify_types ~tolerant:() k k'))
    | k :: {mt = MT1 (Option, v)} :: {mt = MT2 (Map, k', v')} :: _ ->
        Option.map2
          ~f:(fun k v -> [mt_map k v])
          (Result.get_ok (unify_types ~tolerant:() k k'))
          (Result.get_ok (unify_types ~tolerant:() v v'))
    | k :: {mt = MT1 (Option, v)} :: {mt = MT2 (Big_map, k', v')} :: _ ->
        Option.map2
          ~f:(fun k v -> [mt_big_map k v])
          (Result.get_ok (unify_types ~tolerant:() k k'))
          (Result.get_ok (unify_types ~tolerant:() v v'))
    | _ -> None)

let mi_get_and_update =
  mk_spec_basic "GET_AND_UPDATE" ~arities:(3, 2) (function
    | k :: {mt = MT1 (Option, v)} :: {mt = MT2 (Map, k', v')} :: _ ->
        Option.map2
          ~f:(fun k v -> [mt_option v; mt_map k v])
          (Result.get_ok (unify_types ~tolerant:() k k'))
          (Result.get_ok (unify_types ~tolerant:() v v'))
    | k :: {mt = MT1 (Option, v)} :: {mt = MT2 (Big_map, k', v')} :: _ ->
        Option.map2
          ~f:(fun k v -> [mt_option v; mt_big_map k v])
          (Result.get_ok (unify_types ~tolerant:() k k'))
          (Result.get_ok (unify_types ~tolerant:() v v'))
    | _ -> None)

let mi_open_chest =
  mk_spec_basic "OPEN_CHEST" ~arities:(3, 1) (function
    | {mt = MT0 Chest_key} :: {mt = MT0 Chest} :: {mt = MT0 Nat} :: _ ->
        Some [mt_or mt_bytes mt_bool]
    | _ -> None)

let mi_mem =
  mk_spec_basic "MEM" ~arities:(2, 1) (function
    | k :: {mt = MT1 (Set, k')} :: _ when unifiable_types k k' -> Some [mt_bool]
    | k :: {mt = MT2 (Map, k', _)} :: _ when unifiable_types k k' ->
        Some [mt_bool]
    | k :: {mt = MT2 (Big_map, k', _)} :: _ when unifiable_types k k' ->
        Some [mt_bool]
    | _ -> None)

let mi_min_block_time =
  mk_spec_const "MIN_BLOCK_TIME" mt_nat

let mi_exec =
  mk_spec_basic "EXEC" ~arities:(2, 1) (function
    | k :: {mt = MT2 (Lambda, k', v)} :: _ ->
        if unifiable_types k k' then Some [v] else None
    | _ -> None)

let mi_apply =
  mk_spec_basic "APPLY" ~arities:(2, 1) (function
    | k :: {mt = MT2 (Lambda, {mt = MT2 (Pair _, k', k'')}, v)} :: _
      when unifiable_types k k' -> Some [mt_lambda k'' v]
    | _ -> None)

let mi_contract t =
  mk_spec_basic "CONTRACT" ~arities:(1, 1) (function
    | {mt = MT0 Address} :: _ -> Some [mt_option (mt_contract t)]
    | _ -> None)

let mi_view t =
  mk_spec_basic "VIEW" ~arities:(2, 1) (function
    | _ :: {mt = MT0 Address} :: _ -> Some [mt_option t]
    | _ -> None)

let mi_cast t =
  mk_spec_basic "CAST" ~arities:(1, 1) (function
    | t' :: _ when unifiable_types t t' -> Some [t]
    | _ -> None)

let mi_emit t =
  let matches t' =
    match t with
    | None -> true
    | Some t -> unifiable_types t t'
  in
  mk_spec_basic "EMIT" ~arities:(1, 1) (function
    | t' :: _ when matches t' -> Some [mt_operation]
    | _ -> None)

let mi_rename annot_variable =
  mk_spec_basic "RENAME" ~arities:(1, 1) (function
    | t :: _ -> Some [{t with annot_variable}]
    | _ -> None)

let mi_transfer_tokens =
  mk_spec_basic "TRANSFER_TOKENS" ~arities:(3, 1) (function
    | p :: {mt = MT0 Mutez} :: {mt = MT1 (Contract, p')} :: _
      when unifiable_types p p' -> Some [mt_operation]
    | _ -> None)

let mi_set_delegate =
  mk_spec_basic "SET_DELEGATE" ~arities:(1, 1) (function
    | {mt = MT1 (Option, {mt = MT0 Key_hash})} :: _ -> Some [mt_operation]
    | _ -> None)

let mi_sapling_verify_update =
  mk_spec_basic "SAPLING_VERIFY_UPDATE" ~arities:(2, 1) (function
    | {mt = MT0 (Sapling_transaction {memo = m1})}
      :: {mt = MT0 (Sapling_state {memo = m2})}
      :: _
      when m1 = m2 ->
        Some
          [mt_option (mt_pair mt_bytes (mt_pair mt_int (mt_sapling_state m1)))]
    | _ -> None)

let mi_concat1 =
  mk_spec_basic "CONCAT" ~arities:(1, 1) (function
    | {mt = MT1 (List, {mt = MT0 (String | Bytes) as mt})} :: _ ->
        Some [mk_mtype mt]
    | _ -> None)

let mi_concat2 =
  mk_spec_basic "CONCAT" ~arities:(2, 1) (function
    | {mt = MT0 String as mt} :: {mt = MT0 String} :: _
    | {mt = MT0 Bytes as mt} :: {mt = MT0 Bytes} :: _ -> Some [mk_mtype mt]
    | _ -> None)

let mi_concat_unresolved =
  let rule ~tparameter stack instr =
    let _instr1, r1 = mi_concat1.rule ~tparameter stack instr in
    match r1 with
    | Ok _ -> (MI1 Concat1, r1)
    | Error _ ->
        let _instr2, r2 = mi_concat2.rule ~tparameter stack instr in
        (MI2 Concat2, r2)
  in
  {name = "CONCAT"; rule; commutative = false; arities = None}

let mi_pack =
  mk_spec_basic "PACK" ~arities:(1, 1) (function
    | t :: _ when is_packable t -> Some [mt_bytes]
    | _ -> None)

let mi_unpack t =
  mk_spec_basic "UNPACK" ~arities:(1, 1) (function
    | {mt = MT0 Bytes} :: _ when is_packable t -> Some [mt_option t]
    | _ -> None)

let mi_slice =
  mk_spec_basic "SLICE" ~arities:(3, 1) (function
    | {mt = MT0 Nat} :: {mt = MT0 Nat} :: {mt = MT0 String} :: _ ->
        Some [mt_option mt_string]
    | {mt = MT0 Nat} :: {mt = MT0 Nat} :: {mt = MT0 Bytes} :: _ ->
        Some [mt_option mt_bytes]
    | _ -> None)

let mi_size =
  mk_spec_basic "SIZE" ~arities:(1, 1) (function
    | {
        mt =
          ( MT0 String
          | MT0 Bytes
          | MT1 ((Set | List), _)
          | MT2 ((Map | Big_map), _, _) )
      }
      :: _ -> Some [mt_nat]
    | _ -> None)

let mi_mich ~name ~types_in ~types_out =
  mk_spec_basic name
    ~arities:(List.length types_in, List.length types_out)
    (fun stack ->
      if is_prefix equal_mtype types_in stack then Some types_out else None)

let mi_self =
  let rule ~tparameter stack = function
    | MI0 (Self ep_name) -> (
        let tinstr = MI0 (Self ep_name) in
        let mt_contract t =
          {(mt_contract t) with annot_variable = Some "self"}
        in
        match ep_name with
        | None ->
            Some (tinstr, Ok (Stack_ok (mt_contract (fst tparameter) :: stack)))
        | Some ep_name -> (
            let rec find_ep (t, annot) =
              match (t.mt, annot) with
              | _, Some a when a = ep_name -> Some t
              | MT2 (Or {annot_left; annot_right}, left, right), None -> (
                  match find_ep (left, annot_left) with
                  | Some t -> Some t
                  | None -> find_ep (right, annot_right))
              | _ -> None
            in
            match find_ep tparameter with
            | None -> None
            | Some t -> Some (tinstr, Ok (Stack_ok (mt_contract t :: stack)))))
    | _ -> assert false
  in
  mk_spec "SELF" ~arities:(0, 1) rule

let mi_address =
  mk_spec_basic "ADDRESS" ~arities:(1, 1) (function
    | {mt = MT1 (Contract, _)} :: _ -> Some [mt_address]
    | _ -> None)

let mi_implicit_account =
  mk_spec_basic "IMPLICIT_ACCOUNT" ~arities:(1, 1) (function
    | {mt = MT0 Key_hash} :: _ -> Some [mt_contract mt_unit]
    | _ -> None)

let mi_voting_power =
  mk_spec_basic "VOTING_POWER" ~arities:(1, 1) (function
    | {mt = MT0 Key_hash} :: _ -> Some [mt_nat]
    | _ -> None)

let mi_create_contract =
  let rule ~tparameter:_ stack = function
    | MIcreate_contract {tparameter; tstorage; code; views} -> (
        match stack with
        | {mt = MT1 (Option, {mt = MT0 Key_hash})}
          :: {mt = MT0 Mutez}
          :: storage :: tail ->
            let code =
              code (Ok (initial_stack ~tparameter:(fst tparameter) ~tstorage))
            in
            let views = [] in
            if unifiable_types storage tstorage
            then
              let tinstr =
                MIcreate_contract {tparameter; tstorage; code; views}
              in
              let stack = Ok (Stack_ok (mt_operation :: mt_address :: tail)) in
              Some (tinstr, stack)
            else None
        | _ -> None)
    | _ -> assert false
  in
  mk_spec "CREATE_CONTRACT" ~arities:(3, 2) rule

let spec_of_prim0 p =
  let mk name =
    let t = Michelson_base.Typing.type_prim0 p in
    mk_spec_const name t
  in
  match p with
  | Sender -> mk "SENDER"
  | Source -> mk "SOURCE"
  | Amount -> mk "AMOUNT"
  | Balance -> mk "BALANCE"
  | Chain_id -> mk "CHAIN_ID"
  | Level -> mk "LEVEL"
  | Now -> mk "NOW"
  | Total_voting_power -> mk "TOTAL_VOTING_POWER"
  | Self_address -> mk "SELF_ADDRESS"
  | Sapling_empty_state _ -> mk "SAPLING_EMPTY_STATE"
  | Nil t -> mi_nil t
  | Empty_set t -> mi_empty_set t
  | Empty_bigmap (k, v) -> mi_empty_big_map k v
  | Empty_map (k, v) -> mi_empty_map k v
  | None_ t -> mi_none t
  | Unit_ -> mi_unit
  | Self _ -> mi_self
  | Min_block_time -> mi_min_block_time

let spec_of_prim1  p =
  let mk name =
    let t1, t = Michelson_base.Typing.type_prim1 p in
    let f = function
      | x :: _ when unifiable_types x t1 -> Some [t]
      | _ -> None
    in
    mk_spec_basic name ~arities:(1, 1) f
  in
  match p with
  | Hash_key -> mk "HASH_KEY"
  | IsNat -> mk "ISNAT"
  | Blake2b -> mk "BLAKE2B"
  | Sha256 -> mk "SHA256"
  | Sha512 -> mk "SHA512"
  | Keccak -> mk "KECCAK"
  | Sha3 -> mk "SHA3"
  | Abs -> mk "ABS"
  | Not -> mi_not 
  | Contract (_, t) -> mi_contract t
  | Cast t -> mi_cast t
  | Emit (_tag, ty) -> mi_emit ty
  | Rename a -> mi_rename a
  | Concat1 -> mi_concat1
  | Set_delegate -> mi_set_delegate
  | Read_ticket -> mi_read_ticket
  | Join_tickets -> mi_join_tickets
  | Pairing_check -> mi_pairing_check
  | Eq -> mi_eq
  | Neq -> mi_neq
  | Lt -> mi_lt
  | Le -> mi_le
  | Gt -> mi_gt
  | Ge -> mi_ge
  | Neg -> mi_neg
  | Nat -> mi_nat
  | Int -> mi_int
  | Bytes -> mi_bytes
  | Some_ -> mi_some
  | Left (annot_left, annot_right, t) -> mi_left ?annot_left ?annot_right t
  | Right (annot_left, annot_right, t) -> mi_right ?annot_left ?annot_right t
  | Pack -> mi_pack
  | Unpack t -> mi_unpack t
  | Getn n -> mi_getn n
  | Address -> mi_address
  | Implicit_account -> mi_implicit_account
  | Voting_power -> mi_voting_power
  | Size -> mi_size
  | Car | Cdr -> assert false

let spec_of_prim2  p =
  let mk ?commutative name =
    match Michelson_base.Typing.type_prim2 p with
    | [((t1, t2), t)] ->
        let f = function
          | x1 :: x2 :: _ when unifiable_types x1 t1 && unifiable_types x2 t2 ->
              Some [t]
          | _ -> None
        in
        mk_spec_basic name ?commutative ~arities:(2, 1) f
    | instances ->
        let f = function
          | x1 :: x2 :: _ -> (
              match
                assoc_opt (strip_annots x1, strip_annots x2) instances
              with
              | None -> None
              | Some t -> Some [t])
          | _ -> None
        in
        mk_spec_basic name ?commutative ~arities:(2, 1) f
  in
  match p with
  | Lsl -> mi_shift_left 
  | Lsr -> mi_shift_right 
  | Add -> mk ~commutative:() "ADD"
  | Sub -> mi_sub
  | Sub_mutez -> mk "SUB_MUTEZ"
  | Mul -> mk "MUL"
  | Ediv -> mi_ediv
  | Concat2 -> mi_concat2
  | Sapling_verify_update -> mi_sapling_verify_update
  | Ticket -> mi_ticket
  | Ticket_deprecated -> mi_ticket_deprecated
  | Split_ticket -> mi_split_ticket
  | Compare -> mi_compare
  | Pair (annot_fst, annot_snd) -> mi_pair ?annot_fst ?annot_snd ()
  | Cons -> mi_cons
  | And -> mi_and 
  | Or -> mi_or 
  | Xor -> mi_xor 
  | Get -> mi_get
  | Updaten n -> mi_updaten n
  | Mem -> mi_mem
  | Exec -> mi_exec
  | Apply -> mi_apply
  | View (_, t) -> mi_view t

let spec_of_prim3 p =
  let mk name =
    let t1, t2, t3, t = Michelson_base.Typing.type_prim3 p in
    let f = function
      | x1 :: x2 :: x3 :: _
        when unifiable_types x1 t1 && unifiable_types x2 t2
             && unifiable_types x3 t3 -> Some [t]
      | _ -> None
    in
    mk_spec_basic name ~arities:(3, 1) f
  in
  match p with
  | Check_signature -> mk "CHECK_SIGNATURE"
  | Slice -> mi_slice
  | Transfer_tokens -> mi_transfer_tokens
  | Update -> mi_update
  | Get_and_update -> mi_get_and_update
  | Open_chest -> mi_open_chest

let spec_of_instr  = function
  | MI0 p -> spec_of_prim0 p
  | MI1 p -> spec_of_prim1  p
  | MI2 p -> spec_of_prim2  p
  | MI3 p -> spec_of_prim3 p
  | MI1_fail Failwith -> mi_failwith
  | MI1_fail Never -> mi_never
  | MIpush _ -> mi_push
  | MIconcat1 -> mi_concat1
  | MIconcat2 -> mi_concat2
  | MIconcat_unresolved -> mi_concat_unresolved
  | MIConstant _ -> assert false
  | MIswap -> mi_swap
  | MIdrop -> mi_drop
  | MIdropn n -> mi_dropn n
  | MIunpair n -> mi_unpair n
  | MIpairn n -> mi_pairn n
  | MIfield steps -> mi_field steps
  | MIsetField steps -> mi_set_field steps
  | MIdup i -> mi_dup i
  | MIcreate_contract _ -> mi_create_contract
  | MImich {name; typesIn; typesOut} ->
      mi_mich ~name ~types_in:typesIn ~types_out:typesOut
  | MIdip _ -> mi_dip
  | MIdipn _ -> mi_dipn
  | MIiter _ -> mi_iter
  | MImap _ -> mi_map
  | MIloop _ -> mi_loop
  | MIloop_left _ -> mi_loop_left
  | MIif _ -> mi_if
  | MIif_none _ -> mi_if_none
  | MIif_left _ -> mi_if_left
  | MIif_cons _ -> mi_if_cons
  | MIdig n -> mi_dig n
  | MIdug n -> mi_dug n
  | MIseq _ -> mi_seq
  | MIlambda _ -> mi_lambda
  | MIlambda_rec _ -> mi_lambda_rec
  | MIcomment _ -> mi_comment
  | MIerror s -> mi_error s

let is_commutative  instr = (spec_of_instr  instr).commutative

let name_of_instr  instr = (spec_of_instr  instr).name

(** {1 Type checking} *)

(* Match a comb type against the given tuple. *)
let rec match_comb t xs =
  match (t.mt, xs) with
  | _, [x] -> [(t, x)]
  | MT2 (Pair _, fst, snd), x :: xs -> (fst, x) :: match_comb snd xs
  | _ -> failwith "match_comb"

(* Roll a list into a right comb. *)
let rec comb_literal = function
  | [x1; x2] -> {tliteral = Pair (x1, x2); t = Result.map2 mt_pair x1.t x2.t}
  | x :: xs ->
      let xs = comb_literal xs in
      {tliteral = Pair (x, xs); t = Result.map2 mt_pair x.t xs.t}
  | _ -> assert false

let name_of_instr_exn  = function
  | ( MI0
        ( Sender
        | Source
        | Amount
        | Balance
        | Level
        | Now
        | Self _
        | Self_address
        | Chain_id
        | Total_voting_power
        | Sapling_empty_state _
        | Unit_
        | None_ _
        | Nil _
        | Empty_set _
        | Empty_map _
        | Empty_bigmap _
        | Min_block_time )
    | MI1
        ( Car
        | Cdr
        | Left _
        | Right _
        | Some_
        | Eq
        | Abs
        | Neg
        | Int
        | Nat
        | Bytes
        | IsNat
        | Neq
        | Le
        | Lt
        | Ge
        | Gt
        | Not
        | Concat1
        | Size
        | Address
        | Implicit_account
        | Contract _
        | Pack
        | Unpack _
        | Hash_key
        | Blake2b
        | Sha256
        | Sha512
        | Keccak
        | Sha3
        | Set_delegate
        | Read_ticket
        | Join_tickets
        | Pairing_check
        | Voting_power
        | Getn _
        | Cast _
        | Rename _
        | Emit _ )
    | MI1_fail (Failwith | Never)
    | MI2
        ( Pair _
        | Add
        | Mul
        | Sub
        | Sub_mutez
        | Lsr
        | Lsl
        | Xor
        | Ediv
        | And
        | Or
        | Cons
        | Compare
        | Concat2
        | Get
        | Mem
        | Exec
        | Apply
        | Ticket
        | Ticket_deprecated
        | Split_ticket
        | Updaten _
        | View _ )
    | MIdrop | MIswap
    | MI3
        ( Slice
        | Update
        | Get_and_update
        | Transfer_tokens
        | Check_signature
        | Open_chest )
    | MImich _
    | MIdropn _
    | MIdup _
    | MIpush _
    | MIunpair _
    | MIpairn _
    | MIfield _
    | MIsetField _
    | MIconcat1
    | MIconcat2
    | MIconcat_unresolved
    | MIConstant _ ) as instr -> name_of_instr  instr
  | MIdip _ -> "DIP"
  | MIdipn _ -> "DIPN"
  | MIloop _ -> "LOOP"
  | MIloop_left _ -> "LOOP_LEFT"
  | MIiter _ -> "ITER"
  | MImap _ -> "MAP"
  | MIif_left _ -> "IF_LEFT"
  | MIif_none _ -> "IF_NONE"
  | MIif_cons _ -> "IF_CONS"
  | MIif _ -> "IF"
  | MIdig _ -> "DIG"
  | MIdug _ -> "DUG"
  | MIlambda _ -> "LAMBDA"
  | MIlambda_rec _ -> "LAMBDA_REC"
  | MIerror _ -> failwith "name_of_instr_exn: MIerror"
  | MIcomment _ -> failwith "name_of_instr_exn: MIcomment"
  | MIseq _ -> failwith "name_of_instr_exn: MIseq"
  | MIcreate_contract _ -> "CREATE_CONTRACT"
  | MI2 Sapling_verify_update -> "SAPLING_VERIFY_UPDATE"

let two_field_annots = function
  | Some a1, Some a2 -> ["%" ^ a1; "%" ^ a2]
  | Some a1, None -> ["%" ^ a1]
  | None, Some a2 -> ["%"; "%" ^ a2]
  | None, None -> []

let seq_snoc xs x =
  match xs with
  | {tinstr = MIseq xs; stack_in} ->
      {tinstr = MIseq (xs @ [x]); stack_in; stack_out = x.stack_out}
  | xs ->
      {tinstr = MIseq [xs; x]; stack_in = xs.stack_in; stack_out = x.stack_out}

let wrap_in_seq = function
  | {tinstr = MIseq _} as i -> i
  | i -> {tinstr = MIseq [i]; stack_in = i.stack_in; stack_out = i.stack_out}

let insert_subsequences =
  let f_tinstr ~stack_in ~stack_out = function
    | MIseq _ as tinstr -> {tinstr; stack_in; stack_out}
    | tinstr ->
        {tinstr = map_instr_f wrap_in_seq id tinstr; stack_in; stack_out}
  in
  cata_tinstr {f_tinstr; f_tliteral = (fun ~t tliteral -> {tliteral; t})}

type contract = {
    tparameter : mtype * string option
  ; tstorage : mtype
  ; code : instr
  ; views : instr view list
}
[@@deriving show {with_path = false}]

type instance = {
    contract : contract
  ; storage : literal option
}
[@@deriving show {with_path = false}]

type tcontract = {
    tparameter : mtype * string option
  ; tstorage : mtype
  ; code : tinstr
  ; views : tinstr view list
}
[@@deriving show {with_path = false}]

type tinstance = {
    contract : tcontract
  ; storage : tliteral option
}
[@@deriving show {with_path = false}]

module Of_micheline = struct
  open Micheline

  let rec mtype x = fst (mtype_annotated x)

  and mtype_annotated = function
    | Micheline.Primitive {name; annotations; arguments} as p ->
        let mt =
          match (name, arguments) with
          | "pair", [t1; t2] ->
              let fst, annot_fst = mtype_annotated t1 in
              let snd, annot_snd = mtype_annotated t2 in
              mt_pair ?annot_fst ?annot_snd fst snd
          | "pair", l -> (
              match List.rev_map ~f:mtype_annotated l with
              | [] -> assert false
              | (last, annot) :: rest ->
                  fst
                    (List.fold_left
                       ~f:(fun (snd, annot_snd) (fst, annot_fst) ->
                         (mt_pair ?annot_fst ?annot_snd fst snd, None))
                       ~init:(last, annot) rest))
          | "or", [t1; t2] ->
              let left, annot_left = mtype_annotated t1 in
              let right, annot_right = mtype_annotated t2 in
              mt_or ?annot_left ?annot_right left right
          | "unit", [] -> mt_unit
          | "bool", [] -> mt_bool
          | "mutez", [] -> mt_mutez
          | "timestamp", [] -> mt_timestamp
          | "nat", [] -> mt_nat
          | "int", [] -> mt_int
          | "string", [] -> mt_string
          | "key", [] -> mt_key
          | "signature", [] -> mt_signature
          | "bytes", [] -> mt_bytes
          | "chain_id", [] -> mt_chain_id
          | "key_hash", [] -> mt_key_hash
          | "contract", [t] -> mt_contract (mtype t)
          | "address", [] -> mt_address
          | "list", [t] -> mt_list (mtype t)
          | "option", [t] -> mt_option (mtype t)
          | "set", [t] -> mt_set (mtype t)
          | "map", [t1; t2] -> mt_map (mtype t1) (mtype t2)
          | "big_map", [t1; t2] -> mt_big_map (mtype t1) (mtype t2)
          | "lambda", [t1; t2] -> mt_lambda (mtype t1) (mtype t2)
          | "operation", [] -> mt_operation
          | "sapling_state", [Int memo] -> mt_sapling_state (int_of_string memo)
          | "sapling_transaction", [Int memo] ->
              mt_sapling_transaction (int_of_string memo)
          | "never", [] -> mt_never
          | "ticket", [t] -> mt_ticket (mtype t)
          | "bls12_381_g1", [] -> mt_bls12_381_g1
          | "bls12_381_g2", [] -> mt_bls12_381_g2
          | "bls12_381_fr", [] -> mt_bls12_381_fr
          | "chest_key", [] -> mt_chest_key
          | "chest", [] -> mt_chest
          | _ -> mk_mtype (MT_var (sprintf "Parse type error %S" (pretty "" p)))
        in
        List.fold_left
          ~f:(fun (mt, fa) a ->
            let update = function
              | Some _ -> failwith "duplicate annotation"
              | None -> Some (String.sub a 1 (String.length a - 1))
            in
            match a.[0] with
            | ':' -> ({mt with annot_type = update mt.annot_type}, fa)
            | '@' -> ({mt with annot_variable = update mt.annot_variable}, fa)
            | '%' -> (mt, update fa)
            | _ -> failwith "cannot parse annotation")
          ~init:(mt, None) annotations
    | p -> failwith ("Parse type error " ^ pretty "" p)

  let rec literal x : literal =
    match (x : Micheline.t) with
    | Int i -> MLiteral.int (Bigint.of_string ~msg:"michelson" i)
    | Bytes s -> MLiteral.bytes s
    | String s -> MLiteral.string s
    | Primitive {name; annotations = _; arguments} -> (
        match (name, arguments) with
        | "Unit", [] -> MLiteral.unit
        | "False", [] -> MLiteral.bool false
        | "True", [] -> MLiteral.bool true
        | "Pair", l -> (
            match List.rev l with
            | [] -> assert false
            | a :: l ->
                List.fold_left
                  ~f:(fun x y -> MLiteral.pair (literal y) x)
                  ~init:(literal a) l)
        | "None", [] -> MLiteral.none
        | "Some", [x] -> MLiteral.some (literal x)
        | "Left", [x] -> MLiteral.left (literal x)
        | "Right", [x] -> MLiteral.right (literal x)
        | "Elt", [k; v] -> MLiteral.elt (literal k) (literal v)
        | "Lambda_rec", [x] -> MLiteral.lambda_rec (instruction x)
        | "constant", [String hash] -> MLiteral.constant hash
        | _ -> MLiteral.instr (instruction x))
    | Sequence xs -> MLiteral.seq (List.map ~f:literal xs)

  and instruction x =
    let err () =
      MIerror (sprintf "Cannot parse instruction %S" (Micheline.show x))
    in
    let cmp instr = MIseq [{instr = MI2 Compare}; {instr}] in
    let fail =
      MIseq
        [{instr = MIpush (mt_unit, MLiteral.unit)}; {instr = MI1_fail Failwith}]
    in
    let if_op instr x y =
      MIseq [{instr}; {instr = MIif (instruction x, instruction y)}]
    in
    let ifcmp_op instr x y =
      MIseq
        [
          {instr = MI2 Compare}
        ; {instr}
        ; {instr = MIif (instruction x, instruction y)}
        ]
    in
    let assert_op instr =
      MIseq [{instr}; {instr = MIif ({instr = MIseq []}, {instr = fail})}]
    in
    let assert_cmp_op instr =
      MIseq
        [
          {instr = MI2 Compare}
        ; {instr}
        ; {instr = MIif ({instr = MIseq []}, {instr = fail})}
        ]
    in
    let parse_simple_macro = function
      | "FAIL" -> fail
      | "ASSERT" -> MIif ({instr = MIseq []}, {instr = fail})
      | "CMPEQ" -> cmp (MI1 Eq)
      | "CMPNEQ" -> cmp (MI1 Eq)
      | "CMPLT" -> cmp (MI1 Lt)
      | "CMPGT" -> cmp (MI1 Gt)
      | "CMPLE" -> cmp (MI1 Le)
      | "CMPGE" -> cmp (MI1 Ge)
      | "ASSERTEQ" -> assert_op (MI1 Eq)
      | "ASSERTNEQ" -> assert_op (MI1 Eq)
      | "ASSERTLT" -> assert_op (MI1 Lt)
      | "ASSERTGT" -> assert_op (MI1 Gt)
      | "ASSERTLE" -> assert_op (MI1 Le)
      | "ASSERTGE" -> assert_op (MI1 Ge)
      | "ASSERT_CMPEQ" -> assert_cmp_op (MI1 Eq)
      | "ASSERT_CMPNEQ" -> assert_cmp_op (MI1 Eq)
      | "ASSERT_CMPLT" -> assert_cmp_op (MI1 Lt)
      | "ASSERT_CMPGT" -> assert_cmp_op (MI1 Gt)
      | "ASSERT_CMPLE" -> assert_cmp_op (MI1 Le)
      | "ASSERT_CMPGE" -> assert_cmp_op (MI1 Ge)
      | "ASSERT_SOME" -> MIif_none ({instr = fail}, {instr = MIseq []})
      | "ASSERT_NONE" -> MIif_none ({instr = MIseq []}, {instr = fail})
      | prim
        when String.index_opt prim 'D' = Some 0
             && String.index_opt prim 'P' = Some (String.length prim - 1)
             && 2 < String.length prim
             && Base.String.count prim ~f:(fun c -> c = 'U')
                = String.length prim - 2 ->
          let n = String.length prim - 3 in
          MIseq [{instr = MIdig n}; {instr = MIdup 1}; {instr = MIdug (n + 1)}]
      | prim
        when String.index_opt prim 'C' = Some 0
             && String.index_opt prim 'R' = Some (String.length prim - 1) -> (
          let l =
            Base.String.fold
              (String.sub prim 1 (String.length prim - 2))
              ~init:(Some [])
              ~f:(fun acc c ->
                match (acc, c) with
                | Some acc, 'A' -> Some (A :: acc)
                | Some acc, 'D' -> Some (D :: acc)
                | _ -> None)
          in
          match l with
          | Some l -> MIfield (List.rev l)
          | None -> 
            err ())
      | _ -> 
        err ()
    in
    let instr =
      match x with
      | Sequence [x] -> (instruction x).instr
      | Sequence xs -> MIseq (List.map ~f:instruction xs)
      | Primitive {name; annotations; arguments} -> (
          match (name, arguments) with
          | "RENAME", [] ->
              let a =
                match annotations with
                | [] -> None
                | [a] -> Base.String.chop_prefix ~prefix:"@" a
                | _ -> assert false
              in
              MI1 (Rename a)
          | "UNIT", [] -> MI0 Unit_
          | "EMPTY_MAP", [k; v] ->
              MIpush (mt_map (mtype k) (mtype v), MLiteral.mk_map [])
          | "EMPTY_SET", [t] -> MIpush (mt_set (mtype t), MLiteral.set [])
          | "EMPTY_BIG_MAP", [k; v] -> MI0 (Empty_bigmap (mtype k, mtype v))
          | "DIP", [x] -> MIdip (instruction x)
          | "DIP", [Int i; x] -> MIdipn (int_of_string i, instruction x)
          | prim, [x]
            when String.index_opt prim 'D' = Some 0
                 && String.index_opt prim 'P' = Some (String.length prim - 1)
                 && 2 < String.length prim
                 && Base.String.count prim ~f:(fun c -> c = 'I')
                    = String.length prim - 2 ->
              MIdipn (String.length prim - 2, instruction x)
          | "LOOP", [x] -> MIloop (instruction x)
          | "LOOP_LEFT", [x] -> MIloop_left (instruction x)
          | "ITER", [x] -> MIiter (instruction x)
          | "MAP", [x] -> MImap (instruction x)
          | "DROP", [] -> MIdrop
          | "DROP", [Int n] -> MIdropn (int_of_string n)
          | "DUP", [] -> MIdup 1
          | "DUP", [Int n] -> MIdup (int_of_string n)
          | "DIG", [Int i] -> MIdig (int_of_string i)
          | "DUG", [Int i] -> MIdug (int_of_string i)
          | "FAILWITH", [] -> MI1_fail Failwith
          | "IF", [x; y] -> MIif (instruction x, instruction y)
          | "IF_LEFT", [x; y] -> MIif_left (instruction x, instruction y)
          | "IF_RIGHT", [x; y] -> MIif_left (instruction y, instruction x)
          | "IF_SOME", [x; y] -> MIif_none (instruction y, instruction x)
          | "IF_NONE", [x; y] -> MIif_none (instruction x, instruction y)
          | "IF_CONS", [x; y] -> MIif_cons (instruction x, instruction y)
          | "NIL", [t] -> MI0 (Nil (mtype t))
          | "CONS", [] -> MI2 Cons
          | "NONE", [t] -> MI0 (None_ (mtype t))
          | "SOME", [] -> MI1 Some_
          | "PAIR", [] -> MI2 (Pair (None, None))
          | "PAIR", [Int n] -> MIpairn (int_of_string n)
          | "LEFT", [t] -> MI1 (Left (None, None, mtype t))
          | "RIGHT", [t] -> MI1 (Right (None, None, mtype t))
          | "PUSH", [t; l] -> MIpush (mtype t, literal l)
          | "SWAP", [] -> MIswap
          | "UNPAIR", [] -> MIunpair [true; true]
          | "UNPAIR", [Int n] ->
              MIunpair (List.init (int_of_string n) (fun _ -> true))
          | "CAR", [] -> MIfield [A]
          | "CDR", [] -> MIfield [D]
          | "CONTRACT", [t] ->
              let entrypoint =
                match annotations with
                | [] -> None
                | entrypoint :: _ ->
                    Base.String.chop_prefix ~prefix:"%" entrypoint
              in
              MI1 (Contract (entrypoint, mtype t))
          | "VIEW", [String name; t] -> MI2 (View (name, mtype t))
          | "CAST", [t] ->
              let t = mtype t in
              MI1 (Cast t)
          | "EXEC", [] -> MI2 Exec
          | "APPLY", [] -> MI2 Apply
          | "LAMBDA", [t; u; x] -> MIlambda (mtype t, mtype u, instruction x)
          | "LAMBDA_REC", [t; u; x] ->
              MIlambda_rec (mtype t, mtype u, instruction x)
          | "EMIT", _ -> (
              match (annotations, arguments) with
              | [], [] -> MI1 (Emit (None, None))
              | [annot], [] -> MI1 (Emit (Some annot, None))
              | [], [t] -> MI1 (Emit (None, Some (mtype t)))
              | [annot], [t] -> MI1 (Emit (Some annot, Some (mtype t)))
              | _ -> 
                err ())
          | "CREATE_CONTRACT", [x] ->
              let tparameter, tstorage, code =
                if false then failwith (Micheline.show x);
                match x with
                | Sequence
                    [
                      Primitive {name = "parameter"; arguments = [tparameter]}
                    ; Primitive {name = "storage"; arguments = [tstorage]}
                    ; Primitive {name = "code"; arguments = [code]}
                    ] ->
                    ( ( mtype tparameter
                      , None (* TODO single entrypoint annotation *) )
                    , mtype tstorage
                    , instruction code )
                | _ -> assert false
              in
              MIcreate_contract
                {tparameter; tstorage; code; views = (* TODO *) []}
          | "SELF", [] ->
              let entrypoint =
                match annotations with
                | [] -> None
                | entrypoint :: _ ->
                    Base.String.chop_prefix ~prefix:"%" entrypoint
              in
              MI0 (Self entrypoint)
          | "ADDRESS", [] -> MI1 Address
          | "SELF_ADDRESS", [] -> MI0 Self_address
          | "IMPLICIT_ACCOUNT", [] -> MI1 Implicit_account
          | "TRANSFER_TOKENS", [] -> MI3 Transfer_tokens
          | "CHECK_SIGNATURE", [] -> MI3 Check_signature
          | "SET_DELEGATE", [] -> MI1 Set_delegate
          | "SAPLING_EMPTY_STATE", [Int memo] ->
              MI0 (Sapling_empty_state {memo = int_of_string memo})
          | "SAPLING_EMPTY_STATE", [Primitive {name = "int"; annotations = annots; arguments = args}] -> 
            err ()
          | "SAPLING_VERIFY_UPDATE", [] -> MI2 Sapling_verify_update
          | "NEVER", [] -> MI1_fail Never
          | "READ_TICKET", [] -> MI1 Read_ticket
          | "TICKET", [] -> MI2 Ticket
          | "TICKET_DEPRECATED", [] -> MI2 Ticket_deprecated
          | "SPLIT_TICKET", [] -> MI2 Split_ticket
          | "JOIN_TICKETS", [] -> MI1 Join_tickets
          | "PAIRING_CHECK", [] -> MI1 Pairing_check
          | "TOTAL_VOTING_POWER", [] -> MI0 Total_voting_power
          | "VOTING_POWER", [] -> MI1 Voting_power
          | "EQ", [] -> MI1 Eq
          | "NEQ", [] -> MI1 Neq
          | "LE", [] -> MI1 Le
          | "LT", [] -> MI1 Lt
          | "GE", [] -> MI1 Ge
          | "GT", [] -> MI1 Gt
          | "COMPARE", [] -> MI2 Compare
          | "MUL", [] -> MI2 Mul
          | "ADD", [] -> MI2 Add
          | "SUB", [] -> MI2 Sub
          | "EDIV", [] -> MI2 Ediv
          | "NOT", [] -> MI1 Not
          | "AND", [] -> MI2 And
          | "OR", [] -> MI2 Or
          | "LSL", [] -> MI2 Lsl
          | "LSR", [] -> MI2 Lsr
          | "SUB_MUTEZ", [] -> MI2 Sub_mutez
          | "XOR", [] -> MI2 Xor
          | "CONCAT", [] -> MIconcat1 (* Changed from MIconcat_unresolved *)
          | "SLICE", [] -> MI3 Slice
          | "SIZE", [] -> MI1 Size
          | "GET", [] -> MI2 Get
          | "GET", [Int x] -> MI1 (Getn (int_of_string x))
          | "UPDATE", [] -> MI3 Update
          | "UPDATE", [Int x] -> MI2 (Updaten (int_of_string x))
          | "GET_AND_UPDATE", [] -> MI3 Get_and_update
          | "OPEN_CHEST", [] -> MI3 Open_chest
          | "SENDER", [] -> MI0 Sender
          | "SOURCE", [] -> MI0 Source
          | "AMOUNT", [] -> MI0 Amount
          | "BALANCE", [] -> MI0 Balance
          | "NOW", [] -> MI0 Now
          | "LEVEL", [] -> MI0 Level
          | "CHAIN_ID", [] -> MI0 Chain_id
          | "MEM", [] -> MI2 Mem
          | "MIN_BLOCK_TIME", [] -> MI0 Min_block_time
          | "HASH_KEY", [] -> MI1 Hash_key
          | "BLAKE2B", [] -> MI1 Blake2b
          | "SHA256", [] -> MI1 Sha256
          | "SHA512", [] -> MI1 Sha512
          | "KECCAK", [] -> MI1 Keccak
          | "SHA3", [] -> MI1 Sha3
          | "ABS", [] -> MI1 Abs
          | "NEG", [] -> MI1 Neg
          | "INT", [] -> MI1 Int
          | "NAT", [] -> MI1 Nat
          | "BYTES", [] -> MI1 Bytes
          | "ISNAT", [] -> MI1 IsNat
          | "PACK", [] -> MI1 Pack
          | "UNPACK", [t] -> MI1 (Unpack (mtype t))
          | prim, [] -> parse_simple_macro prim
          | "IFEQ", [x; y] -> if_op (MI1 Eq) x y
          | "IFNEQ", [x; y] -> if_op (MI1 Eq) x y
          | "IFLT", [x; y] -> if_op (MI1 Lt) x y
          | "IFGT", [x; y] -> if_op (MI1 Gt) x y
          | "IFLE", [x; y] -> if_op (MI1 Le) x y
          | "IFGE", [x; y] -> if_op (MI1 Ge) x y
          | "IFCMPEQ", [x; y] -> ifcmp_op (MI1 Eq) x y
          | "IFCMPNEQ", [x; y] -> ifcmp_op (MI1 Eq) x y
          | "IFCMPLT", [x; y] -> ifcmp_op (MI1 Lt) x y
          | "IFCMPGT", [x; y] -> ifcmp_op (MI1 Gt) x y
          | "IFCMPLE", [x; y] -> ifcmp_op (MI1 Le) x y
          | "IFCMPGE", [x; y] -> ifcmp_op (MI1 Ge) x y
          (* TODO Macros: ASSERT_SOME, ASSERT_LEFT, ASSERT_RIGHT *)
          (*Global constant*)
          | "constant", [hash] -> MIConstant (literal hash)
          | _ -> 
            err ())
      | _ -> 
        err ()
    in
    {instr}

  let contract =
    let read_element (p, s, c, vs) = function
      | Micheline.Primitive {name = "parameter"; arguments} -> (
          match (p, arguments) with
          | None, [parameter] ->
              let parameter = mtype_annotated parameter in
              (Some parameter, s, c, vs)
          | None, _ -> failwith "ill-formed 'parameter'"
          | Some _, _ -> failwith "'parameter' defined twice")
      | Primitive {name = "storage"; arguments} -> (
          match (s, arguments) with
          | None, [storage] ->
              let storage = mtype storage in
              (p, Some storage, c, vs)
          | None, _ -> failwith "ill-formed 'storage'"
          | Some _, _ -> failwith "'storage' defined twice")
      | Primitive {name = "code"; arguments} -> (
          match (c, arguments) with
          | None, [code] ->
              let code = instruction code in
              (p, s, Some code, vs)
          | None, _ -> failwith "ill-formed 'code'"
          | Some _, _ -> failwith "'code' defined twice")
      | Primitive {name = "view"; arguments} -> (
          match arguments with
          | [String name; tin; tout; code] ->
              let tin = mtype tin in
              let tout = mtype tout in
              let code = instruction code in
              let (view : instr view) =
                {
                  name
                ; pure = true
                ; doc = ""
                ; tparameter = Some tin
                ; treturn = tout
                ; onchain_code = Some code
                ; offchain_code = code
                }
              in
              (p, s, c, view :: vs)
          | _ -> failwith "ill-formed 'view'")
      | Primitive {name} ->
          failwith ("ill-formed contract: unexpected '" ^ name ^ "'")
      | _ -> failwith "ill-formed contract: expected primitive"
    in
    function
    | Micheline.Sequence s -> (
        match List.fold_left ~f:read_element ~init:(None, None, None, []) s with
        | None, _, _, _ -> failwith "ill-formed contract: missing 'parameter'"
        | _, None, _, _ -> failwith "ill-formed contract: missing 'storage'"
        | _, _, None, _ -> failwith "ill-formed contract: missing 'code'"
        | Some tparameter, Some tstorage, Some code, views ->
            let views = List.rev views in
            ({contract = {tparameter; tstorage; code; views}; storage = None}
              : instance))
    | _ -> failwith "ill-formed contract: not a sequence"
end

module To_micheline = struct
  open Micheline

  let mtype ?annot_field =
    cata_mtype ~annot_field (fun ?annot_type ?annot_variable t ~annot_field ->
        let annotations =
          let get pref = Option.map ~f:(( ^ ) pref) in
          somes
            [get "%" annot_field; get ":" annot_type; get "@" annot_variable]
        in
        let prim = primitive ~annotations in
        match t with
        | MT0 t ->
            let t, memo = string_of_type0 t in
            option_cata (prim t []) (fun x -> prim t [Int x]) memo
        | MT1 (t, t1) -> prim (string_of_type1 t) [t1 ~annot_field:None]
        | MT2 (t, t1, t2) ->
            let t, a1, a2 = string_of_type2 t in
            prim t [t1 ~annot_field:a1; t2 ~annot_field:a2]
        | MT_var msg ->
            primitive "ERROR"
              [Format.kasprintf string "Cannot compile missing type: %s" msg])

  let dip_seq i = primitive "DIP" [sequence i]

  let rec c_ad_r s =
    (*
         See http://tezos.gitlab.io/mainnet/whitedoc/michelson.html#syntactic-conveniences
         > CA(\rest=[AD]+)R / S  =>  CAR ; C(\rest)R / S
         > CD(\rest=[AD]+)R / S  =>  CDR ; C(\rest)R / S
      *)
    match s.[0] with
    | 'A' -> primitive "CAR" [] :: c_ad_r (Base.String.drop_prefix s 1)
    | 'D' -> primitive "CDR" [] :: c_ad_r (Base.String.drop_prefix s 1)
    | exception _ -> []
    | other ->
        Format.kasprintf failwith "c_ad_r macro: wrong char: '%c' (of %S)" other
          s

  let rec set_c_ad_r s =
    (*
         See http://tezos.gitlab.io/mainnet/whitedoc/michelson.html#syntactic-conveniences
         > SET_CA(\rest=[AD]+)R / S   =>
             { DUP ; DIP { CAR ; SET_C(\rest)R } ; CDR ; SWAP ; PAIR } / S
         > SET_CD(\rest=[AD]+)R / S   =>
             { DUP ; DIP { CDR ; SET_C(\rest)R } ; CAR ; PAIR } / S
         Then,
         > SET_CAR  =>  CDR ; SWAP ; PAIR
         > SET_CDR  =>  CAR ; PAIR
      *)
    match s.[0] with
    | 'A' when String.length s > 1 ->
        [
          primitive "DUP" []
        ; dip_seq
            (primitive "CAR" [] :: set_c_ad_r (Base.String.drop_prefix s 1))
        ; primitive "CDR" []
        ; primitive "SWAP" []
        ; primitive "PAIR" []
        ]
    | 'A' -> [primitive "CDR" []; primitive "SWAP" []; primitive "PAIR" []]
    | 'D' when String.length s > 1 ->
        [
          primitive "DUP" []
        ; dip_seq
            (primitive "CDR" [] :: set_c_ad_r (Base.String.drop_prefix s 1))
        ; primitive "CAR" []
        ; primitive "PAIR" []
        ]
    | 'D' -> [primitive "CAR" []; primitive "PAIR" []]
    | exception _ ->
        Format.kasprintf failwith "set_c_r_macro: called with no chars: S" s
    | other ->
        Format.kasprintf failwith "set_c_r_macro: wrong char: '%c' (of %S)"
          other s

  let rec literal  {literal = l} =
    let literal = literal  in
    let instruction = instruction  in
    match l with
    | Int bi -> int (Big_int.string_of_big_int bi)
    | Bool false -> primitive "False" []
    | Bool true -> primitive "True" []
    | String s -> string s
    | Unit -> primitive "Unit" []
    | Bytes b -> bytes b
    | Pair (left, right) -> primitive "Pair" [literal left; literal right]
    | None_ -> primitive "None" []
    | Some_ l -> primitive "Some" [literal l]
    | Left e -> primitive "Left" [literal e]
    | Right e -> primitive "Right" [literal e]
    | Seq xs -> xs |> List.map ~f:literal |> sequence
    | Elt (k, v) -> primitive "Elt" [literal k; literal v]
    | Instr x -> sequence (instruction x)
    | Lambda_rec x -> primitive "Lambda_rec" (instruction x)
    | AnyMap xs ->
        let xs = List.map ~f:(fun (k, v) -> {literal = Elt (k, v)}) xs in
        literal {literal = Seq xs}
    | Constant hash -> primitive "constant" [string hash]

  and instruction  (the_instruction : instr) =
    let literal = literal  in
    let instruction = instruction  in
    let prim0 ?annotations n = [primitive ?annotations n []] in
    let primn ?annotations n l = [primitive ?annotations n l] in
    let rec_instruction instr = Micheline.sequence (instruction instr) in
    match the_instruction.instr with
    | MIerror s -> primn "ERROR" [string s]
    | MIcomment _comment ->
        []
        (*
          [ primitive "PUSH" [primitive "string" []; string comment]
          ; primitive "DROP" [] ] *)
    | MIdip instr -> primn "DIP" [rec_instruction instr]
    | MIdipn (n, instr) ->
        primn "DIP" [int (string_of_int n); rec_instruction instr]
    | MIdig n -> primn "DIG" [int (string_of_int n)]
    | MIdug n -> primn "DUG" [int (string_of_int n)]
    | MIdup 1 -> primn "DUP" []
    | MIdup n -> primn "DUP" [int (string_of_int n)]
    | MIunpair [true; true] -> primn "UNPAIR" []
    | MIunpair n -> primn "UNPAIR" [int (unpair_arg n)]
    | MIpairn n -> primn "PAIR" [int (string_of_int n)]
    | MI1 (Getn n) -> primn "GET" [int (string_of_int n)]
    | MI2 (Updaten n) -> primn "UPDATE" [int (string_of_int n)]
    | MIdropn n -> primn "DROP" [int (string_of_int n)]
    | MIloop instr -> primn "LOOP" [rec_instruction instr]
    | MIloop_left instr -> primn "LOOP_LEFT" [rec_instruction instr]
    | MIiter instr -> primn "ITER" [rec_instruction instr]
    | MImap instr -> primn "MAP" [rec_instruction instr]
    | MIseq ils -> List.concat_map ~f:instruction ils
    | MIif (t, e) -> primn "IF" [rec_instruction t; rec_instruction e]
    | MIif_left (t, e) -> primn "IF_LEFT" [rec_instruction t; rec_instruction e]
    | MIif_none (t, e) -> primn "IF_NONE" [rec_instruction t; rec_instruction e]
    | MIif_cons (t, e) -> primn "IF_CONS" [rec_instruction t; rec_instruction e]
    | MIpush (mt, lit) -> primn "PUSH" [mtype mt; literal lit]
    | MI0 (Self (Some entrypoint)) ->
        primn ~annotations:["%" ^ entrypoint] "SELF" []
    | MI2 (Pair (a1, a2)) ->
        primn ~annotations:(two_field_annots (a1, a2)) "PAIR" []
    | MI1 (Right (a1, a2, mty)) ->
        primn ~annotations:(two_field_annots (a1, a2)) "RIGHT" [mtype mty]
    | MI1 (Left (a1, a2, mty)) ->
        primn ~annotations:(two_field_annots (a1, a2)) "LEFT" [mtype mty]
    | MI0 (None_ mty) -> primn "NONE" [mtype mty]
    | MI0 (Nil mty) -> primn "NIL" [mtype mty]
    | MI0 (Empty_set mty) -> primn "EMPTY_SET" [mtype mty]
    | MI0 (Empty_map (k, v)) -> primn "EMPTY_MAP" [mtype k; mtype v]
    | MI0 (Empty_bigmap (k, v)) -> primn "EMPTY_BIG_MAP" [mtype k; mtype v]
    | MI1 (Contract (None, mty)) -> primn "CONTRACT" [mtype mty]
    | MI1 (Contract (Some entrypoint, mty)) ->
        primn ~annotations:["%" ^ entrypoint] "CONTRACT" [mtype mty]
    | MI2 (View (name, mty)) -> primn "VIEW" [string name; mtype mty]
    | MI1 (Cast t) -> primn "CAST" [mtype t]
    | MI1 (Rename None) -> primn "RENAME" []
    | MI1 (Rename (Some a)) -> primn "RENAME" ~annotations:["@" ^ a] []
    | MIlambda (t1, t2, b) ->
        primn "LAMBDA" [mtype t1; mtype t2; rec_instruction b]
    | MIlambda_rec (t1, t2, b) ->
        primn "LAMBDA_REC" [mtype t1; mtype t2; rec_instruction b]
    | MI1 (Emit (tag, ty)) ->
        let annotations =
          match tag with
          | None -> []
          | Some tag -> [tag]
        in
        let ty =
          match ty with
          | None -> []
          | Some ty -> [mtype ty]
        in
        primn ~annotations "EMIT" ty
    | MI1 (Unpack mty) -> primn "UNPACK" [mtype mty]
    | MIfield op -> c_ad_r (string_of_ad_path op)
    | MIsetField op -> set_c_ad_r (string_of_ad_path op)
    | MIcreate_contract
        {tparameter = tparameter, annot_field; tstorage; code; views} ->
        primn "CREATE_CONTRACT"
          [
            sequence
              (primn "parameter" [mtype ?annot_field tparameter]
              @ primn "storage" [mtype tstorage]
              @ primn "code" [rec_instruction code]
              @ List.concat_map ~f:(view ) views)
          ]
    | MI0 (Sapling_empty_state {memo}) ->
        primn "SAPLING_EMPTY_STATE" [int (string_of_int memo)]
    | MIConstant hash -> primn "constant" [literal hash]
    | ( MI0
          ( Sender
          | Source
          | Amount
          | Balance
          | Level
          | Now
          | Self _
          | Self_address
          | Chain_id
          | Total_voting_power
          | Unit_ 
          | Min_block_time)
      | MI1
          ( Car
          | Cdr
          | Some_
          | Eq
          | Abs
          | Neg
          | Int
          | Nat
          | Bytes
          | IsNat
          | Neq
          | Le
          | Lt
          | Ge
          | Gt
          | Not
          | Concat1
          | Size
          | Address
          | Implicit_account
          | Pack
          | Hash_key
          | Blake2b
          | Sha256
          | Sha512
          | Keccak
          | Sha3
          | Set_delegate
          | Read_ticket
          | Join_tickets
          | Pairing_check
          | Voting_power )
      | MI1_fail (Failwith | Never)
      | MI2
          ( Add
          | Mul
          | Sub
          | Sub_mutez
          | Lsr
          | Lsl
          | Xor
          | Ediv
          | And
          | Or
          | Cons
          | Compare
          | Concat2
          | Get
          | Mem
          | Exec
          | Apply
          | Sapling_verify_update
          | Ticket
          | Ticket_deprecated
          | Split_ticket )
      | MI3
          ( Slice
          | Update
          | Get_and_update
          | Transfer_tokens
          | Check_signature
          | Open_chest )
      | MIdrop | MIswap | MImich _ | MIconcat1 | MIconcat2 | MIconcat_unresolved ) as simple -> (
        try prim0 (name_of_instr_exn  simple)
        with _ -> [sequence [primitive "ERROR-NOT-SIMPLE" []]])

  and view  {name; tparameter; treturn; onchain_code} =
    let open Micheline in
    match onchain_code with
    | None -> []
    | Some code ->
        [
          primitive "view"
            [
              literal  {literal = String name}
            ; mtype (default mt_unit tparameter)
            ; mtype treturn
            ; sequence (instruction  code)
            ]
        ]
end

let profile_of_arity (m, n) = (m, Some (n - m))

let arity_of_profile = function
  | m, None -> (m, None)
  | m, Some d -> (m, Some (m + d))

let profile  =
  let open Result in
  let ( =?= ) x y =
    match x with
    | Some x when x <> y -> error "profile: unequal p"
    | _ -> return ()
  in
  let if_some d x = Option.map ~f:(fun _ -> x) d in
  let same x y =
    match (x, y) with
    | Some x, Some y ->
        if x = y then return (Some x) else error "profile: unequal d"
    | None, Some y -> return (Some y)
    | Some x, None -> return (Some x)
    | None, None -> return None
  in
  let pred = Option.map ~f:(fun x -> x - 1) in
  let succ = Option.map ~f:(fun x -> x + 1) in
  let f_instr i =
    let* i = sequence_instr_f i in
    match i with
    | MI1_fail _ -> return (1, None)
    | MIdip (p, d) -> return (p + 1, d)
    | MIdipn (i, (p, d)) -> return (p + i, d)
    | MIloop (p, d) ->
        let* () = d =?= 1 in
        return (p + 1, if_some d (-1))
    | MIloop_left (p, d) ->
        let* () = d =?= 1 in
        return (max 1 p, if_some d 0)
    | MIiter (p, d) ->
        let* () = d =?= -1 in
        return (p, Some (-1))
    | MImap (p, d) ->
        let* () = d =?= 0 in
        return (p, if_some d 0)
    | MIdig n | MIdug n -> return (n + 1, Some 0)
    | MIif ((p1, d1), (p2, d2)) ->
        let* d = same d1 d2 in
        return (max p1 p2 + 1, pred d)
    | MIif_none ((p1, d1), (p2, d2)) ->
        let* d = same (pred d1) d2 in
        return (max (p1 + 1) p2, d)
    | MIif_left ((p1, d1), (p2, d2)) ->
        let* d = same d1 d2 in
        return (max p1 p2, d)
    | MIif_cons ((p1, d1), (p2, d2)) ->
        let* d = same (succ d1) (pred d2) in
        return (max (p1 - 1) (p2 + 1), d)
    | MIseq xs ->
        let f = function
          | _, Error e -> Error e
          | (p1, None), _ -> return (p1, None)
          (* TODO Fix the compiler/rewriter such that they never emit
             instructions after FAILWITH and assert false here. *)
          | (p1, Some d1), Ok (p2, Some d2) ->
              return (max p1 (p2 - d1), Some (d1 + d2))
          | (p1, Some d1), Ok (p2, None) -> return (max p1 (p2 - d1), None)
        in
        List.fold_right ~f:(curry f) xs ~init:(return (0, Some 0))
    | MIcomment _ -> return (0, Some 0)
    | MIlambda _ -> return (0, Some 1)
    | MIlambda_rec _ -> return (0, Some 1)
    | MIconcat_unresolved -> failwith "profile: CONCAT arity undetermined"
    | MIConstant _ -> assert false
    | MIerror _ -> return (0, Some 0)
    | i -> (
        match spec_of_instr  i with
        | {arities = Some a} -> return (profile_of_arity a)
        | _ -> assert false)
  in
  cata_instr {f_instr; f_literal = (fun _ -> return ())}

let has_profile  pr instr = Ok pr = profile  {instr}

let has_arity  a instr =
  has_profile  (profile_of_arity a) instr
