open Tezos_micheline.Micheline
open Tezos_utils.Michelson

let is_cond = function
  | "IF" | "IF_NONE" | "IF_CONS" | "IF_LEFT" -> true
  | _ -> false

(* If we have no type info, we use is_injective to decide which instructions are
   "harmless" enough to attempt merging out of the IF. *)
let is_injective : string -> bool = function
  | "DUP" | "SWAP" | "DIG" | "DUG"
  | "CREATE_ACCOUNT" | "SET_DELEGATE"
  | "RENAME" | "UNPACK" | "BLAKE2B" | "SHA256" | "SHA512"
  | "ABS" | "AMOUNT" | "BALANCE" | "CHECK_SIGNATURE"
  | "CONS" | "IMPLICIT_ACCOUNT" | "EMPTY_MAP" | "EMPTY_SET"
  | "HASH_KEY" | "INT" | "LAMBDA" | "LEFT"
  | "LSL" | "LSR" | "NIL" | "NONE" | "NOW"
  | "PAIR" | "PUSH" | "RIGHT" | "SOME" | "SOURCE" | "SENDER"
  | "SELF" | "SLICE" | "STEPS_TO_QUOTA" | "UNIT" 
  | "XOR" | "CONTRACT" | "ISNAT" | "CHAIN_ID"
  | "EMPTY_BIG_MAP" -> true
  | _ -> false

(* If pre_type is Some, we skip injectivity checks and allow all instructions,
   otherwise we require is_injective = true for merges. *)
let filter_instr (instr : string) : bool = is_injective instr
  
(** The sets “force_nil”, “force_z”, “no_force”. 
    If a given instruction name is in these sets, we consider it for merging. **)
let build_force_sets =
  let f = is_injective in
  let force_nil = 
    List.filter f
      [ "SWAP"
      ; "PAIR"
      ; "UNPAIR"
      ; "CAR"
      ; "CDR"
      ; "DUP"
      ; "DROP"
      ; "UNIT"
      ; "SOME"
      ; "CONS"
      ; "SIZE"
      ; "UPDATE"
      ; "ADD"
      ; "SUB"
      ; "MUL"
      ; "EDIV"
      ; "ABS"
      ; "ISNAT"
      ; "INT"
      ; "NEG"
      ; "LSL"
      ; "LSR"
      ; "OR"
      ; "AND"
      ; "XOR"
      ; "NOT"
      ; "COMPARE"
      ; "EQ"
      ; "NEQ"
      ; "LT"
      ; "GT"
      ; "LE"
      ; "GE"
      ; "SLICE"
      ; "CONCAT"
      ; "PACK"
      ; "SENDER"
      ; "AMOUNT"
      ; "ADDRESS"
      ; "SOURCE"
      ; "BALANCE"
      ; "LEVEL"
      ; "NOW"
      ]
  in
  let force_z =
    List.filter f
      [ "PAIR"; "UNPAIR"; "CAR"; "CDR"; "DUP"; "DROP"; "DIG"; "DUG"; "UPDATE" ]
  in
  let no_force =
    List.filter f
      [ "NIL"; "NONE"; "LEFT"; "RIGHT" ]
  in
  (force_nil, force_z, no_force)


(** Finding the last instruction, removing the last instruction. **)
let rec last_instruction = function
  | Seq (_, []) -> None
  | Seq (_, [x]) -> last_instruction x
  | Seq (l, _ :: xs) -> last_instruction (Seq (l, xs))
  | prim -> Some prim

let rec remove_last (pred : _ michelson -> bool) (node : _ michelson) : _ michelson =
  match node with
  | Seq (loc, []) -> Seq (loc, [])
  | Seq (loc, items) ->
    begin
      match List.rev items with
      | [] -> Seq (loc, [])
      | last :: rev_rest ->
        (match last with
         | Prim (l, p, [bt; bf], annot) when is_cond p ->
           (* Recurse inside the branches of the IF. *)
           let bt' = remove_last pred bt in
           let bf' = remove_last pred bf in
           let new_if = Prim (l, p, [bt'; bf'], annot) in
           Seq (loc, List.rev (new_if :: rev_rest))

         | x when pred x ->
           (* remove it from the sequence tail *)
           Seq (loc, List.rev rev_rest)

         | Seq _ ->
           (* Recurse inside that sub-Seq. *)
           let replaced = remove_last pred last in
           Seq (loc, List.rev (replaced :: rev_rest))

         | _ ->
           Seq (loc, items)
        )
    end
  | prim -> prim


(** The equality checks from `opt_cond`. We want to see if
   two instructions can be considered "equal" for the purpose of hoisting. **)

(* If [pre_type] is None, we do "injective" checks. 
   If [pre_type] is Some, we do eq_type.  *)

(* We only consider instructions that appear in (force_nil @ force_z @ no_force). *)
let mk_pred (force_nil, force_z, no_force) =
  fun x ->
    match x with
    | Prim (_, name, _, _) ->
      (* Is it in any of these combined sets? *)
      List.mem name (force_nil @ force_z @ no_force)
    | _ -> false

(* 3) The eq check used by last_is eq pred. 
   - If it’s in `force_nil` or `no_force` with no sub-args => we unify if same name + eq_type 
   - If it’s in `force_z` with an [Int (_, n)] => unify if same name + same int + eq_type 
   - else false
*)
let mk_eq (force_nil, force_z, no_force) =
  fun m1 m2 ->
    match m1, m2 with
    (* force_nil, no sub-args, same name => eq_type => true *)
    | Prim (_, l, [], _), Prim (_, r, [], _)
      when List.mem l force_nil && String.equal l r ->
      true

    (* no_force, any sub-args, same name => eq_type => true *)
    | Prim (_, l, subl, _), Prim (_, r, subr, _)
      when List.mem l no_force && String.equal l r ->
      (match subl, subr with
       | [], [] -> true
       | _ -> true) 
    
    (* force_z with [Int (_, n)] => unify if same name + same int + eq_type *)
    | Prim (_, l, [ Int (_, n) ], _),
      Prim (_, r, [ Int (_, m) ], _)
      when List.mem l force_z
           && String.equal l r
           && Z.equal n m ->
      true

    | _ -> false

(** The “opt_cond” pass that merges last instructions from IF branches. **)
let rec opt_cond_one_pass ?pre_type (node : 'l michelson)
  : bool * 'l michelson
=
  match node with
  | Seq (loc, items) ->
    (* recursively optimize each child *)
    let changed, new_items =
      List.fold_left
        (fun (acc_changed, acc_nodes) itm ->
          let changed_i, opt_i = opt_cond_one_pass ?pre_type itm in
          (acc_changed || changed_i, acc_nodes @ [opt_i]))
        (false, [])
        items
    in
    (changed, Seq (loc, new_items))

  | Prim (loc, p, [bt; bf], annot) when is_cond p ->
    (* Recursively optimize sub-branches first. *)
    let cb, bt = opt_cond_one_pass ?pre_type bt in
    let cb2, bf = opt_cond_one_pass ?pre_type bf in
    let branch_changed = cb || cb2 in

    let (force_nil, force_z, no_force) = build_force_sets in
    let eq_fun = mk_eq (force_nil, force_z, no_force) in

    (* last_instruction on each branch *)
    let last_bt = last_instruction bt in
    let last_bf = last_instruction bf in

    begin match last_bt, last_bf with
    | Some li_bt, Some li_bf when eq_fun li_bt li_bf ->
      (* They match => remove from both, then place after the IF. *)
      let bt' = remove_last (fun x -> eq_fun x li_bt) bt in
      let bf' = remove_last (fun x -> eq_fun x li_bf) bf in

      let changed = true in
      let new_if = Prim (loc, p, [bt'; bf'], annot) in

      (* Return “IF ... ; common_instr” as a Seq. *)
      (changed, Seq (loc, [ new_if; li_bt ]))

    | _ ->
      (branch_changed, Prim (loc, p, [bt; bf], annot))
    end

  | Prim (loc, name, items, annot) ->
    (* Recurse into sub-items. *)
    let changed, new_items =
      List.fold_left
        (fun (acc_changed, acc_nodes) itm ->
          let changed_i, opt_i = opt_cond_one_pass ?pre_type itm in
          (acc_changed || changed_i, acc_nodes @ [opt_i]))
        (false, [])
        items
    in
    (changed, Prim (loc, name, new_items, annot))

  | other -> (false, other)


(** Repeatedly apply [opt_cond_one_pass] until stable, 
    plus optional flattening if desired. **)
let rec flatten_seqs ~has_comment = function
  | Seq (loc, items) ->
    let items = List.map (flatten_seqs ~has_comment) items in
    let unseq = function
      | Seq (m, args) when not (has_comment m) -> args
      | x -> [ x ]
    in
    let items = List.concat_map unseq items in
    Seq (loc, items)
  | Prim (loc, p, items, ann) ->
    Prim (loc, p, List.map (flatten_seqs ~has_comment) items, ann)
  | x -> x

let rec fixpoint (f : 'l michelson -> bool * 'l michelson) (node : 'l michelson)
  : 'l michelson
=
  let changed, node' = f node in
  if changed then fixpoint f node' else node'

let optimize
  ?(experimental_disable_optimizations_for_debugging=false)
  ?(has_comment=(fun _ -> false))
  ?pre_type
  (code : 'l michelson)
  : 'l michelson
=
  let code = flatten_seqs ~has_comment code in

  if experimental_disable_optimizations_for_debugging
  then 
    code
  else 
    (* Repeatedly apply opt_cond_one_pass, passing along ~pre_type. *)
    flatten_seqs ~has_comment (fixpoint (opt_cond_one_pass ?pre_type) code)
  
  