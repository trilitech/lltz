(* This file defines modules used for creating sequences of Michelson instructions during LLTZ compilation. It defines
   Stack modules that specifies current types of slots (variable identificator or value, heap unused in LLTZ).
   Each instruction defined in this file takes a stack and returns a new Config with updated stack and instructions.
   The Config module is used to store the current stack and instructions and handling exeception cases where shape of the stack
   does not match instruction requirements.
*)

module M = Michelson.Ast
module I = M.Instruction
module T = M.Type

type t = Stack.t -> Config.t

module SlotStack = Stack
open Core

let noop stack = Config.ok stack []

let seq (ts : t list) : t =
  fun stack ->
  List.fold_left
    ts
    ~init:(noop stack)
    ~f:(fun ({ instructions = instrs1; stack } as config : Config.t) t ->
      match stack with
      | Ok stack ->
        let { Config.stack; instructions = instrs2 } = t stack in
        Config.{ stack; instructions = instrs1 @ instrs2 }
      | Exceptional -> config)

let rev_prefix n l =
  let rec aux n acc l =
    if n = 0
    then acc, l
    else (
      match l with
      | hd :: tl -> aux (n - 1) (hd :: acc) tl
      | [] -> acc, [])
  in
  aux n [] l

(* https://tezos.gitlab.io/michelson-reference/#instr-DIG *)
let dig n stack =
  (* brings element at index n to the top *)
  let stack =
    match rev_prefix n stack with
    | rev_slots1, nth :: slots2 -> nth::List.rev_append rev_slots1 (slots2)
    | _ ->
      raise_s [%message "Instruction.dig: invalid stack" (stack : SlotStack.t) (n : int)]
  in
  Config.ok stack
  @@
  match n with
  | 0 -> []
  | 1 -> [ I.swap ]
  | n -> [ I.dig_n n ]

let dig_and_keep n stack = 
  (* brings element at index n to the top *)
  let stack =
    match rev_prefix n stack with
    | rev_slots1, _ :: slots2 -> `Value::(List.rev_append rev_slots1 slots2)
    | _ ->
      raise_s [%message "Instruction.dig_and_keep: invalid stack" (stack : SlotStack.t) (n : int)]
  in
  Config.ok stack
  @@
  match n with
  | 0 -> []
  | 1 -> [ I.swap; ]
  | n -> [ I.dig_n n;]

(* https://tezos.gitlab.io/michelson-reference/#instr-DUG *)
let dug n stack =
  (* n is a index into the stack, tos will be at index n after instr *)
  let stack =
    match List.split_n stack (n + 1) with
    | nth :: slots1, slots2 -> slots1 @ (nth :: slots2)
    | _ ->
      raise_s [%message "Instruction.dug: invalid stack" (stack : SlotStack.t) (n : int)]
  in
  Config.ok stack
  @@
  match n with
  | 0 -> []
  | 1 -> [ I.swap ]
  | n -> [ I.dug_n n ]

(* https://tezos.gitlab.io/michelson-reference/#instr-DUP, stack is 0-indexed *)
let dup n stack =
  (* n is a index into the stack *)
  let stack =
    if List.length stack >= n
    then `Value :: stack
    else
      raise_s [%message "Instruction.dup: invalid stack" (stack : SlotStack.t) (n : int)]
  in
  Config.ok stack [ I.dup_n (n + 1) ]

(* https://tezos.gitlab.io/michelson-reference/#instr-SWAP *)
let swap stack =
  let stack =
    match stack with
    | s0 :: s1 :: slots -> s1 :: s0 :: slots
    | _ -> raise_s [%message "Instruction.swap: invalid stack" (stack : SlotStack.t)]
  in
  Config.ok stack [ I.swap ]

(* https://tezos.gitlab.io/michelson-reference/#instr-DROP *)
let drop n stack =
  (* drop's n elements *)
  if List.length stack < n then
    raise_s [%message "Instruction.drop: invalid stack" (stack : SlotStack.t) (n : int)]
  else
    let dropped_stack = List.drop stack n in
    let instructions = match n with
      | 0 -> [] (* noop *)
      | 1 -> [ I.drop ] (* drop just one element *)
      | _ -> [ I.drop_n n ] (* drop n elements *)
    in
    Config.ok dropped_stack instructions

let remove n stack =
  (* removes element at index n *)
  let stack =
    match List.split_n stack n with
    | left, _nth :: right -> left @ right
    | _ ->
      raise_s
        [%message "Instruction.remove: invalid stack" (stack : SlotStack.t) (n : int)]
  in
  Config.ok stack
  @@
  match n with
  | 0 -> I.[ drop ]
  | 1 -> I.[ swap; drop ]
  | n -> I.[ dig_n n; drop ]

(* prim m n instr stack: m elements are consumed from the stack, n elements are produced *)
let prim ?(message = "") m n instr stack =
  let stack =
    let left, right = List.split_n stack m in
    (* split stack into left and right at index m *)
    if not
         (List.for_all left ~f:(function
           | `Value -> true
           | _ -> false))
    then
      raise_s
        [%message
          "Instruction.prim: invalid stack" (stack : SlotStack.t) (m : int) (n : int) (message : string)];
    List.init n ~f:(fun _ -> `Value) @ right
  in
  Config.ok stack [ instr ]

let noop stack = Config.ok stack []

(* https://tezos.gitlab.io/michelson-reference/#instr-LOOP *)
let loop (in_ : t) stack =
  let stack =
    match stack with
    | `Value :: stack -> stack
    | _ -> raise_s [%message "Instruction.loop" (stack : SlotStack.t)]
  in
  let instrs = Config.instructions @@ in_ stack in
  Config.ok stack [ I.loop instrs ]

(* https://tezos.gitlab.io/michelson-reference/#instr-LOOP_LEFT *)
let loop_left (in_ : t) stack =
  let stack =
    match stack with
    | `Value :: _ -> stack
    | _ -> raise_s [%message "Instruction.loop_left" (stack : SlotStack.t)]
  in
  let instrs = Config.instructions @@ in_ stack in
  Config.ok stack [ I.loop_left instrs ]

(* https://tezos.gitlab.io/michelson-reference/#instr-ITER *)
let iter (in_ : t) stack =
  match stack with
  | `Value :: stack -> 
    let instrs = Config.instructions @@ in_ (`Value :: stack) in
    Config.ok stack [ I.iter instrs ]
  | _ -> raise_s [%message "Instruction.iter" (stack : SlotStack.t)]

(* https://tezos.gitlab.io/michelson-reference/#instr-MAP *)
let map_ in_ stack =
  match stack with
  | `Value :: _ ->
    let instrs = Config.instructions @@ in_ stack in
    Config.ok stack [ I.map instrs ]
  | _ -> raise_s [%message "Instruction.map" (stack : SlotStack.t)]

(* https://tezos.gitlab.io/michelson-reference/#instr-IF *)
let if_ ~then_ ~else_ stack =
  let stack =
    match stack with
    | `Value :: stack -> stack
    | _ -> raise_s [%message "Instruction.if_" (stack : SlotStack.t)]
  in
  Config.merge (then_ stack) (else_ stack) ~f:(fun instrs1 instrs2 ->
    [ I.if_ ~then_:instrs1 ~else_:instrs2 ])

(* https://tezos.gitlab.io/michelson-reference/#instr-IF_LEFT *)
let if_left ~left ~right stack =
  match stack with
  | `Value :: _ -> 
    Config.merge (left stack) (right stack) ~f:(fun instrs1 instrs2 ->
      [ I.if_left ~then_:instrs1 ~else_:instrs2 ])
  | _ -> raise_s [%message "Instruction.if_left" (stack : SlotStack.t)]

(* https://tezos.gitlab.io/michelson-reference/#instr-IF_CONS *)
let if_cons ~empty ~nonempty stack =
  let stack =
    match stack with
    | `Value :: stack -> stack
    | _ -> raise_s [%message "Instruction.if_cons" (stack : SlotStack.t)]
  in
  Config.merge
    (nonempty (`Value :: `Value :: stack))
    (empty stack)
    ~f:(fun instrs1 instrs2 -> [ I.if_cons ~then_:instrs1 ~else_:instrs2 ])

(* https://tezos.gitlab.io/michelson-reference/#instr-IF_NONE *)
let if_none ~none ~some stack =
  match stack with
  | `Value :: stack -> 
    Config.merge
      (none stack)
      (some (`Value :: stack))
      ~f:(fun instrs1 instrs2 -> [ I.if_none ~then_:instrs1 ~else_:instrs2 ])
  | _ -> raise_s [%message "Instruction.if_none" (stack : SlotStack.t)]

module Slot = struct
  let def (slot : [< Slot.definable ]) ~in_ stack =
    match stack with
    | slot' :: stack when Slot.is_assignable slot' ~to_:slot -> 
      in_ ((slot :> Slot.t) :: stack)
    | [] -> 
      raise_s [%message "Instruction.Slot.bind: invalid stack" (stack : SlotStack.t) (slot : [< Slot.definable ])]
    | _ -> 
      raise_s [%message "Instruction.Slot.def: slot not assignable" (stack : SlotStack.t) (slot : [< Slot.definable ])]

  (* Remove slot from stack if not collected already, for example after it is used by let*)
  let collect slot stack =
    match SlotStack.find stack slot with
    | Some idx -> remove idx stack
    | None -> Config.ok stack []

  let collect_if_unused unused_set slot =
    match slot with
      | `Ident name -> (if Set.mem unused_set name then (collect slot) else noop)
      | _ -> noop

  let let_ slot ?(unused_set = String.Set.empty) ~in_  = 
    let collect_immediately = collect_if_unused unused_set slot in
    seq [ def slot ~in_:(seq [collect_immediately; in_;]); collect slot ] (* bind and remove after used*)

  let def_all slots' ~in_ stack =
    let stack =
      let check_slots slots =
        if not
             (List.for_all slots ~f:(fun (slot, slot') ->
                Slot.is_assignable slot ~to_:slot'))
        then
          raise_s
            [%message
              "Instruction.Slot.def_all: invalid slots"
                (stack : SlotStack.t)
                (slots' : [< Slot.definable ] list)]
      in
      (*
         Zip the stack with the slots, if the stack is longer than the slots, the remaining stack is returned.
         If the stack is shorter than the slots, an exception is raiseds.
      *)
      match List.zip_with_remainder stack slots' with
      | slots, None ->
        check_slots slots;
        let slots = (slots :> (Slot.t * Slot.t) list) in
        List.map ~f:snd slots
      | slots, Some (First right) ->
        check_slots slots;
        let slots = (slots :> (Slot.t * Slot.t) list) in
        List.map ~f:snd slots @ right
      | _ ->
        raise_s [%message "Instruction.Slot.def_all: invalid stack" (stack : SlotStack.t)]
    in
    in_ stack

  let collect_all slots = seq (List.map slots ~f:collect)

  let let_all slots ?(unused_set = String.Set.empty) ~in_ = 
    let collect_immediately = seq (List.map slots ~f:(collect_if_unused unused_set)) in
    seq [ def_all slots ~in_:(seq [collect_immediately; in_;]); collect_all slots ]

  let lookup slot ~in_ stack =
    match SlotStack.find stack slot with
    | Some idx -> in_ idx stack
    | None ->
      raise_s [%message "Instruction.Slot.lookup: slot not found" (stack : SlotStack.t) (slot : [< Slot.definable ])]

  let dup_or_dig slot condition =
    lookup slot ~in_:(fun idx->
      if condition
      then dup idx
      else dig_and_keep idx )
  
  let dup slot = 
    lookup slot ~in_:(fun idx -> dup idx)

  let set (slot : [< Slot.definable ]) stack =
    match SlotStack.find stack slot with
    | Some idx ->
      (* If [slot] is found at position [idx], replace its value *)
      begin
        match idx with
        | 0 -> assert false
        | n ->
          let instructions = I.[ dug_n n; dig_n (n - 1); drop ] in
          match List.split_n stack idx with
          | _val_ :: left, nth :: right ->
            Config.ok (left @ (nth :: right)) instructions
          | _ ->
            raise_s
              [%message
                "Instruction.Slot.set: invalid stack"
                  (stack : SlotStack.t)
                  (slot : [< Slot.definable ])]
      end
  
    | None ->
      (* If [slot] is not found, treat the *top of the stack* as the new [slot] *)
      match stack with
      | top :: rest ->
        if Slot.is_assignable top ~to_:slot then
          Config.ok ((slot :> Slot.t) :: rest) []
        else
          raise_s
            [%message
              "Instruction.Slot.set: top of stack not assignable to slot"
                (top : Slot.t)
                (slot : [< Slot.definable ])]
      | [] ->
        raise_s
          [%message
            "Instruction.Slot.set: stack empty, cannot assign slot"
              (slot : [< Slot.definable ])]
              
  
  let mock_value stack =
    Config.ok (`Value :: stack) []

end

(* https://tezos.gitlab.io/michelson-reference/#instr-DIP *)
let dip n (t : t) : t =
  fun stack ->
  match n with
  | 0 -> t stack (* noop *)
  | n ->
    let rev_left_stack, right_stack = rev_prefix n stack in
    let { Config.stack = right_stack; instructions } = t right_stack in
    let stack =
      Config.ExtStack.apply right_stack ~f:(fun right_stack ->
        List.rev_append rev_left_stack right_stack)
    in
    { Config.stack
    ; instructions =
        [ (match n with
           | 1 -> I.dip instructions
           | n -> I.dip_n n instructions)
        ]
    }

(* https://tezos.gitlab.io/michelson-reference/#instr-UNPAIR, accepts 0 and 1 *)
let unpair_n n =
  (* invariant: pop 1 value, push n *)
  match n with
  | 0 -> drop 1
  | 1 -> noop
  | 2 -> prim 1 2 I.unpair
  | n -> prim 1 n (I.unpair_n n)

(* https://tezos.gitlab.io/michelson-reference/#instr-PAIR, accepts 0 and 1 *)
let pair_n n =
  (* invariant: pop n values, push 1 *)
  match n with
  | 0 -> prim 0 1 I.unit
  | 1 -> noop
  | 2 -> prim ~message:"pair_n" 2 1 (I.pair ())
  | n -> prim n 1 (I.pair_n n)

(* https://tezos.gitlab.io/michelson-reference/#instr-GET *)
(* Instead of using right-comb, the n represents just the number of leaves *)
let get_n idx ~length:n =
  match n with
  | 0 ->
    raise_s [%message "Comp.Instruction.tuple_project: cannot project from empty tuple"]
  | 1 ->
    assert (idx = 0);
    noop
  | n ->
    (* n >= 2 *)
    (* Convert idx to a right-comb index, needs special handling of last value *)
    let k = if idx = n - 1 then 2 * idx else (2 * idx) + 1 in
    prim 1 1 (I.get_n k)

(* https://tezos.gitlab.io/michelson-reference/#instr-UPDATE *)
(* Instead of using right-comb, the n represents just the number of leaves *)
let update_n idx ~length:n =
  (* expected stack: value, tuple, ... *)
  (* result stack: tuple' *)
  match n with
  | 0 -> raise_s [%message "Com.Instruction.tuple_update: cannot update the empty tuple"]
  | 1 ->
    assert (idx = 0);
    remove 1
  | n ->
    let k = if idx = n - 1 then 2 * idx else (2 * idx) + 1 in
    prim ~message:"update_n" 2 1 (I.update_n k)

(* https://tezos.gitlab.io/michelson-reference/#instr-LAMBDA *)
(*  Lambdas used in LLTZ-IR do not use heaps. *)
(* The environment specifies free variables that are used in the lambda.
   The environment variables need to be pushed on the stack before calling the lambda. *)
let lambda ~environment ~lam_var ~return_type return stack =
  let n = (List.length environment) + 1 in
  let environment_slots = List.map environment ~f:(fun (ident, _) -> `Ident ident) in
  let parameter_slot = `Ident (fst lam_var) in

  let lambda_stack = [ `Value ] in
  let { Config.stack = _; instructions } =
    let defined_slots = environment_slots @ [parameter_slot] in
    seq
      [ unpair_n n
      ; Slot.def_all (defined_slots) ~in_:return
      ; Slot.collect_all (defined_slots)
      ]
      lambda_stack
  in
  let parameter_type =
    Type.tuple (List.map environment ~f:snd @ [snd lam_var])
  in
  Config.ok (`Value :: stack) [ I.lambda parameter_type return_type instructions ]

let lambda_raw ~parameter_type ~return_type return stack =
  let lambda_stack = [ `Value ] in
  let { Config.stack = _; instructions } = return lambda_stack in
  Config.ok (`Value :: stack) [ I.lambda parameter_type return_type instructions ]

(* https://tezos.gitlab.io/michelson-reference/#instr-LAMBDA_REC *)
(* Recursive version of lambda, mu is the name of the recursive variable *)

(* Temporarily makes top of the stack a value *)
(* This is done so that we can modify Lambda_rec's mu which is present on the stack as an ident (partial_apps are applied to both an Ident and a Value). *)
let make_top_value in_ stack =
  match stack with
  | `Ident name :: stack -> 
    let { Config.stack = _; instructions } = in_ (`Value :: stack) in
    Config.ok ((`Ident name) :: stack) instructions
  | _ -> 
    raise_s [%message "Instruction.make_top_value: invalid stack" (stack : SlotStack.t)]

let lambda_rec ~environment ~lam_var ~mu ~return_type ~partial_apps return stack =
  let n = (List.length environment) + 1 in
  let environment_slots = List.map environment ~f:(fun (ident, _) -> `Ident ident) in
  let parameter_slot = `Ident (fst lam_var) in

  let lambda_stack = [ `Value; `Value ] in
  let { Config.stack = _; instructions } =
    let defined_slots = environment_slots @ [parameter_slot; `Ident mu] in
    seq
      [ 
      unpair_n n
      ; Slot.def_all (defined_slots) ~in_:
        (seq [
          dig n
          ; make_top_value partial_apps
          ; dug n
          ; return
        ])
      ; Slot.collect_all (defined_slots)
      ]
      lambda_stack
  in
  let parameter_type =
    Type.tuple (List.map environment ~f:snd @ [snd lam_var])
  in
  Config.ok (`Value :: stack) [ I.lambda_rec parameter_type return_type instructions ]

(* https://tezos.gitlab.io/michelson-reference/#instr-NEVER *)
let never stack =
  match stack with
  | `Value :: _ -> Config.raise [ I.never ]
  | _ -> raise_s [%message "Instruction.never: invalid stack" (stack : SlotStack.t)]

(* https://tezos.gitlab.io/michelson-reference/#instr-FAILWITH *)
let failwith stack =
  match stack with
  | `Value :: _ -> Config.raise [ I.failwith ]
  | _ -> raise_s [%message "Instruction.failwith: invalid stack" (stack : SlotStack.t)]

(* https://tezos.gitlab.io/michelson-reference/#instr-CREATE_CONTRACT *)
let create_contract ~storage ~parameter ~code stack =
  match stack with
  | `Value :: `Value :: `Value :: stack -> 
    Config.ok (`Value :: `Value :: stack) [ I.create_contract parameter storage (code (`Value :: stack)) ]
  | _ ->
    raise_s
      [%message "Instruction.create_contract: invalid stack" (stack : SlotStack.t)]

let is_nat = prim 1 1 I.is_nat (* https://tezos.gitlab.io/michelson-reference/#instr-ISNAT *)
let unit = prim 0 1 I.unit (* https://tezos.gitlab.io/michelson-reference/#instr-UNIT *)
let push type_ lit = prim 0 1 (I.push type_ lit) (* https://tezos.gitlab.io/michelson-reference/#instr-PUSH *)
let apply = prim ~message:"apply" 2 1 I.apply (* https://tezos.gitlab.io/michelson-reference/#instr-APPLY *)
let exec = prim ~message:"exec" 2 1 I.exec (* https://tezos.gitlab.io/michelson-reference/#instr-EXEC *)
let unpair = prim 1 2 I.unpair (* https://tezos.gitlab.io/michelson-reference/#instr-UNPAIR *)
let pack = prim 1 1 I.pack (* https://tezos.gitlab.io/michelson-reference/#instr-PACK *)
let some = prim 1 1 I.some (* https://tezos.gitlab.io/michelson-reference/#instr-SOME *)
let update = prim 3 1 I.update (* https://tezos.gitlab.io/michelson-reference/#instr-UPDATE *)
let add = prim ~message:"add" 2 1 I.add (* https://tezos.gitlab.io/michelson-reference/#instr-ADD *)
let sub = prim ~message:"sub" 2 1 I.sub (* https://tezos.gitlab.io/michelson-reference/#instr-SUB *)
let mul = prim ~message:"mul" 2 1 I.mul (* https://tezos.gitlab.io/michelson-reference/#instr-MUL *)
let pair = prim ~message:"pair" 2 1 (I.pair ()) (* https://tezos.gitlab.io/michelson-reference/#instr-PAIR *)
let car = prim 1 1 I.car (* https://tezos.gitlab.io/michelson-reference/#instr-CAR *)
let cdr = prim 1 1 I.cdr (* https://tezos.gitlab.io/michelson-reference/#instr-CDR *)
let get = prim ~message:"get" 2 1 I.get (* https://tezos.gitlab.io/michelson-reference/#instr-GET *)
let unpack type_ = prim 1 1 (I.unpack type_) (* https://tezos.gitlab.io/michelson-reference/#instr-UNPACK *)
let neg = prim 1 1 I.neg (* https://tezos.gitlab.io/michelson-reference/#instr-NEG *)
let ediv = prim ~message:"ediv" 2 1 I.ediv (* https://tezos.gitlab.io/michelson-reference/#instr-EDIV *)

let failwithf fmt =
  Fmt.kstr (fun msg -> seq [ push (T.string ()) (M.string msg); failwith ]) fmt

let and_ = prim ~message:"and_" 2 1 I.and_ (* https://tezos.gitlab.io/michelson-reference/#instr-AND *)
let or_ = prim ~message:"or_" 2 1 I.or_ (* https://tezos.gitlab.io/michelson-reference/#instr-OR *)
let not = prim 1 1 I.not  (* https://tezos.gitlab.io/michelson-reference/#instr-NOT *)
let left type_ = prim 1 1 (I.left type_) (* https://tezos.gitlab.io/michelson-reference/#instr-LEFT *)
let right type_ = prim 1 1 (I.right type_) (* https://tezos.gitlab.io/michelson-reference/#instr-RIGHT *)
let empty_map key_type val_type = prim 0 1 (I.empty_map key_type val_type) (* https://tezos.gitlab.io/michelson-reference/#instr-EMPTY_MAP *)
let compare = prim ~message:"compare" 2 1 I.compare (* https://tezos.gitlab.io/michelson-reference/#instr-COMPARE *)
let eq = prim 1 1 I.eq (* https://tezos.gitlab.io/michelson-reference/#instr-EQ *)
let neq = prim 1 1 I.neq (* https://tezos.gitlab.io/michelson-reference/#instr-NEQ *)
let lt = prim 1 1 I.lt (* https://tezos.gitlab.io/michelson-reference/#instr-LT *)
let le = prim 1 1 I.le (* https://tezos.gitlab.io/michelson-reference/#instr-LE *)
let gt = prim 1 1 I.gt (* https://tezos.gitlab.io/michelson-reference/#instr-GT *)
let ge = prim 1 1 I.ge (* https://tezos.gitlab.io/michelson-reference/#instr-GE *)
let int = prim 1 1 I.int (* https://tezos.gitlab.io/michelson-reference/#instr-INT *)
let nil type_ = prim 0 1 (I.nil type_) (* https://tezos.gitlab.io/michelson-reference/#instr-NIL *)
let cons = prim ~message:"cons" 2 1 I.cons (* https://tezos.gitlab.io/michelson-reference/#instr-CONS *)

let debug = ref false
let next_trace_point = ref (-1)

(* Directly using michelson specified via micheline. Can take arbitrary number of args and return a single value. *)
let rec micheline_fails (node : (unit, Michelson.Ast.Prim.t) Tezos_micheline.Micheline.node) =
  match node with
  | Prim (_, I Failwith, _, _) -> true
  | Prim (_, I Never, _, _ ) -> true
  | Seq (_, xs) -> (
      match List.last xs with
      | Some x -> micheline_fails x
      | None -> false)
  | Prim (_, I If, [l; r], _) | Prim (_, I If_none, [l; r], _) | Prim (_, I If_left, [l; r], _) | Prim (_, I If_cons, [l; r], _) ->
      micheline_fails l && micheline_fails r
  | Prim (_, I Map, [x], _) | Prim (_, I Iter, [x], _) | Prim (_, I Loop, [x], _) | Prim (_, I Dip, [x], _) | Prim (_, I Dip, [_;x], _) ->
      micheline_fails x
  | Prim (_, _, _, _) -> false
  | Int (_, _) | String (_, _) | Bytes (_, _) -> false

let raw_michelson michelson args stack =
  let n = List.length args in
  if List.length stack < n then
    raise_s [%message "Instruction.raw_michelson: invalid stack" (stack : SlotStack.t) (n : int)]
  else
    let top_elements = List.take stack n in
    if List.for_all top_elements ~f:(function `Value -> true | _ -> false) then
      let new_stack = `Value :: (List.drop stack n) in
      if micheline_fails michelson then
        Config.raise [michelson]
      else
        let result_val = Config.ok new_stack [michelson] in 
        result_val
    else
      raise_s [%message "Instruction.raw_michelson: invalid stack" (stack : SlotStack.t) (n : int)]

let global_constant hash args stack =
  raw_michelson (I.global_constant hash) args stack

let set_debug next in_ =
  let curr = !debug in
  debug := next;
  let result = in_ () in
  debug := curr;
  result

let to_michelson t stack ~debug =
  set_debug debug (fun () ->
    let Config.{ stack = _; instructions } = t stack in
    instructions)

let print_s sexp stack =
  if !debug then print_s sexp;
  Config.ok stack []

let trace ?(flag = "") t =
  let trace_point =
    incr next_trace_point;
    !next_trace_point
  in
  seq
    [ (fun stack ->
        print_s [%message (stack : SlotStack.t) (String.concat [(Int.to_string trace_point); "=before "; flag;])] stack)
    ; t
    ; (fun stack ->
        print_s [%message (stack : SlotStack.t) (String.concat [(Int.to_string trace_point); "=after "; flag;])] stack)
    ]
