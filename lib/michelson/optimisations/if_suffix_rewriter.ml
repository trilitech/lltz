(*open Tezos_micheline.Micheline
open Tezos_utils.Michelson

(* Detect whether a primitive is an IF-like conditional. *)
let is_cond = function
  | "IF" | "IF_NONE" | "IF_CONS" | "IF_LEFT" -> true
  | _ -> false

(* Return the last instruction of a node (if any). This is just a 
   single-instruction function, not multi-peep. *)
let rec last_instruction = function
  | Seq (_, []) -> None
  | Seq (loc, [x]) -> last_instruction x
  | Seq (loc, _ :: xs) -> last_instruction (Seq (loc, xs))
  | prim -> Some prim  (* if it's not a Seq, it's "the last instruction" *)

(* Remove the last instruction from a node if it matches [pred]. *)
let rec remove_last pred = function
  | Seq (loc, []) -> Seq (loc, [])
  | Seq (loc, items) ->
    begin match List.rev items with
    | [] -> Seq (loc, [])
    | last :: rev_rest ->
      match last with
      | Prim (l, p, [bt; bf], annot) when is_cond p ->
        let bt' = remove_last pred bt in
        let bf' = remove_last pred bf in
        let seq' = Seq (loc, List.rev (Prim (l, p, [bt'; bf'], annot) :: rev_rest)) in
        seq'
      | x when pred x ->
        Seq (loc, List.rev rev_rest)  (* drop the last item *)
      | Seq _ as seq_inner ->
        let replaced = remove_last pred seq_inner in
        Seq (loc, List.rev (replaced :: rev_rest))
      | _ ->
        Seq (loc, items)  (* not removed *)
    end
  | x -> x

(* For your “common-tail” optimization, we do: 
   1) recursively optimize both branches, 
   2) check if they end with the same instruction, 
   3) if so, hoist it. *)

(* A small helper for checking whether two instructions are “equal enough”
   to hoist. You had more advanced logic with typed info. This is a 
   simplified (untyped) approach. *)

let eq_michelson (n1 : 'l michelson) (n2 : 'l michelson) : bool =
  (* Simple structural equality check. 
     If you need typed-based or partial equality, adapt accordingly. *)
  let rec eq_node x y =
    match x, y with
    | Int (_, i), Int (_, j) -> Z.equal i j
    | String (_, s), String (_, t) -> String.equal s t
    | Bytes (_, b), Bytes (_, c) -> Bytes.equal b c
    | Seq (_, xs), Seq (_, ys) -> eq_list xs ys
    | Prim (_, p, xs, ann), Prim (_, q, ys, ann2) ->
      String.equal p q && ann = ann2 && eq_list xs ys
    | _ -> false
  and eq_list xs ys =
    let len_x = List.length xs in
    let len_y = List.length ys in
    len_x = len_y && List.for_all2 eq_node xs ys
  in
  eq_node n1 n2

(* The set of instructions for which we want to check “common tail.” 
   This is your `force_nil @ force_z @ no_force` logic, or you can 
   restrict it. *)
let relevant_instrs =
  [ "SWAP"; "PAIR"; "UNPAIR"; "CAR"; "CDR"; "DUP"; "DROP";
    "UNIT"; "SOME"; "CONS"; "SIZE"; "UPDATE"; "ADD"; "SUB"; "MUL";
    "EDIV"; "ABS"; "ISNAT"; "INT"; "NEG"; "LSL"; "LSR"; "OR"; "AND";
    "XOR"; "NOT"; "COMPARE"; "EQ"; "NEQ"; "LT"; "GT"; "LE"; "GE";
    "SLICE"; "CONCAT"; "PACK"; "SENDER"; "AMOUNT"; "ADDRESS"; "SOURCE";
    "BALANCE"; "LEVEL"; "NOW"; "NIL"; "NONE"; "LEFT"; "RIGHT";
    "DIG"; "DUG" ]

let is_relevant_instr = function
  | Prim (_, p, _, _) -> List.mem p relevant_instrs
  | _ -> false

(* Attempt to hoist common tail from a single IF. 
   - Optimize sub-branches first,
   - if last instructions are the same, remove them from both branches, 
     then place after the IF. 
   Returns: (changed, new_node). *)
let rec opt_cond_one_pass (node : 'l michelson) : bool * 'l michelson =
  match node with
  | Seq (loc, items) ->
    let changed_list, new_items =
      List.fold_left
        (fun (any_change, acc) itm ->
           let changed_i, opt_i = opt_cond_one_pass itm in
           (any_change || changed_i, acc @ [ opt_i ]))
        (false, [])
        items
    in
    (changed_list, Seq (loc, new_items))

  | Prim (loc, p, [ bt; bf ], annot) when is_cond p ->
    (* Recursively optimize the branches first. *)
    let changed_bt, bt = opt_cond_one_pass bt in
    let changed_bf, bf = opt_cond_one_pass bf in
    let branch_changed = changed_bt || changed_bf in

    (* Check the last instructions of each branch. *)
    begin match last_instruction bt, last_instruction bf with
    | Some li_bt, Some li_bf 
      when is_relevant_instr li_bt 
        && is_relevant_instr li_bf
        && eq_michelson li_bt li_bf ->
      (* They match, so remove from both branches. *)
      let bt' = remove_last (fun x -> eq_michelson x li_bt) bt in
      let bf' = remove_last (fun x -> eq_michelson x li_bf) bf in
      let changed = true in
      let if_node = Prim (loc, p, [ bt'; bf' ], annot) in
      (changed, Seq (loc, [ if_node; li_bt ]))
    | _ ->
      (branch_changed, Prim (loc, p, [ bt; bf ], annot))
    end

  | Prim (loc, p, items, annot) ->
    (* Recurse inside the sub-items. *)
    let changed_list, new_items =
      List.fold_left
        (fun (any_change, acc) itm ->
           let changed_i, opt_i = opt_cond_one_pass itm in
           (any_change || changed_i, acc @ [ opt_i ]))
        (false, [])
        items
    in
    (changed_list, Prim (loc, p, new_items, annot))

  | other ->
    (false, other)

(* Repeatedly apply opt_cond_one_pass until stable. *)
let rec optimize (node : 'l michelson) =
  let changed, node' = opt_cond_one_pass node in
  if changed then optimize node' else node'*)

  open Tezos_micheline.Micheline
  open Tezos_utils.Michelson
  
  (** PART 1: Helper detection functions, from the old code **)
  
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
  let filter_instr ~pre_type (instr : string) : bool = is_injective instr
  
  (** PART 2: The sets “force_nil”, “force_z”, “no_force” from the old code. 
      If a given instruction name is in these sets, we consider it for merging. **)
  
  let build_force_sets ~pre_type =
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
  
  
  (** PART 3: Finding the last instruction, removing the last instruction. **)
  
  let rec last_instruction = function
    | Seq (_, []) -> None
    | Seq (l, [x]) -> last_instruction x
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
  
  
  (** PART 4: The equality checks from the old `opt_cond`. We want to see if
     two instructions can be considered "equal" for the purpose of hoisting. **)
  
  (* If [pre_type] is None, we do "injective" checks. 
     If [pre_type] is Some, we do eq_type. 
     This is a minor adaptation from the old code. *)
  
  (* 2) We replicate the old "pred" check for last instructions. 
     We only consider instructions that appear in (force_nil @ force_z @ no_force). *)
  
  let mk_pred (force_nil, force_z, no_force) =
    fun x ->
      match x with
      | Prim (_, name, _, _) ->
        (* Is it in any of these combined sets? *)
        List.mem name (force_nil @ force_z @ no_force)
      | _ -> false
  
  (* 3) The eq check used by last_is eq pred. We replicate the old pattern: 
     - If it’s in `force_nil` or `no_force` with no sub-args => we unify if same name + eq_type 
     - If it’s in `force_z` with an [Int (_, n)] => unify if same name + same int + eq_type 
     - else false
  *)
  let mk_eq (force_nil, force_z, no_force) ~pre_type =
    fun m1 m2 ->
      match m1, m2 with
      (* force_nil, no sub-args, same name => eq_type => true *)
      | Prim (ll, l, [], _), Prim (lr, r, [], _)
        when List.mem l force_nil && String.equal l r ->
        true
  
      (* no_force, any sub-args, same name => eq_type => true *)
      | Prim (ll, l, subl, _), Prim (lr, r, subr, _)
        when List.mem l no_force && String.equal l r ->
        (* the old code doesn't deeply check subl/subr except via eq_type. 
           But typically NIL, NONE, LEFT, RIGHT might have sub-args. *)
        (match subl, subr with
         | [], [] -> true
         | _ -> true)  (* could refine if you want exact structural match. *)
      
      (* force_z with [Int (_, n)] => unify if same name + same int + eq_type *)
      | Prim (ll, l, [ Int (_, n) ], _),
        Prim (lr, r, [ Int (_, m) ], _)
        when List.mem l force_z
             && String.equal l r
             && Z.equal n m ->
        true
  
      | _ -> false
  
  (** PART 5: The actual “opt_cond” pass that merges last instructions from IF branches. **)
  
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
  
      (* We replicate the old code’s last_is eq pred approach. *)
      let (force_nil, force_z, no_force) = build_force_sets ~pre_type in
      let eq_fun = mk_eq (force_nil, force_z, no_force) ~pre_type in
      let pred_fun = mk_pred (force_nil, force_z, no_force) in
  
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
  
  
  (** PART 6: Repeatedly apply [opt_cond_one_pass] until stable, 
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
    (* Possibly flatten the SEQ nodes, like the original. *)
    let code = flatten_seqs ~has_comment code in
  
    if experimental_disable_optimizations_for_debugging
    then 
      (* If debugging => skip merges. Just return code. *)
      code
    else 
      (* Repeatedly apply opt_cond_one_pass, 
         passing along ~pre_type. *)
         flatten_seqs ~has_comment (fixpoint (opt_cond_one_pass ?pre_type) code)
  
  