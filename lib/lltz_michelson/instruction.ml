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
    | rev_slots1, nth :: slots2 -> List.rev_append rev_slots1 (nth :: slots2)
    | _ ->
      raise_s [%message "Instruction.dig: invalid stack" (stack : SlotStack.t) (n : int)]
  in
  Config.ok stack
  @@
  match n with
  | 0 -> []
  | 1 -> [ I.swap ]
  | n -> [ I.dig_n n ]

(* https://tezos.gitlab.io/michelson-reference/#instr-DUG *)
let dug n stack =
  (* n is a index into the stack, tos will be at index n after instr *)
  let stack =
    match List.split_n stack (n + 1) with
    | nth :: slots1, slots2 -> slots1 @ (nth :: slots2)
    | _ ->
      raise_s [%message "Instruction.dig: invalid stack" (stack : SlotStack.t) (n : int)]
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
let prim m n instr stack =
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
          "Instruction.prim: invalid stack" (stack : SlotStack.t) (m : int) (n : int)];
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
  | `Value :: _ -> 
    let instrs = Config.instructions @@ in_ stack in
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
    (empty stack)
    (nonempty (`Value :: `Value :: stack))
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
      raise_s [%message "Instruction.Slot.def: slot not assignable"]

  (* remove slot from stack, for example after it is used by let*)
  let collect slot stack =
    let found_slot = SlotStack.find_exn stack slot in
    remove found_slot stack

  let let_ slot ~in_ = seq [ def slot ~in_; collect slot ] (* bind and remove after used*)

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
  let let_all slots ~in_ = seq [ def_all slots ~in_; collect_all slots ]
  (* bind and remove after used*)

  let lookup slot ~in_ stack =
    let idx = SlotStack.find_exn stack slot in
    in_ idx stack

  let dup slot = lookup slot ~in_:(fun idx -> dup idx)

  let set slot stack =
    lookup slot stack ~in_:(fun idx stack ->
      let instructions =
        match idx with
        | 0 -> assert false
        | n -> I.[ dug_n n; dig_n (n - 1); drop ]
      in
      match List.split_n stack idx with
      | _val_ :: left, nth :: right -> Config.ok (left @ (nth :: right)) instructions
      | _ ->
        raise_s
          [%message
            "Instruction.Slot.set: invalid stack"
              (stack : SlotStack.t)
              (slot : [< Slot.definable ])])
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
  | 2 -> prim 2 1 I.pair
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
    prim 2 1 (I.update_n k)

(* https://tezos.gitlab.io/michelson-reference/#instr-LAMBDA *)
(*  Lambdas used in LLTZ-IR do not use heaps. *)
let lambda ~lam_var ~return_type return stack =
  let parameter_slot = `Ident (fst lam_var) in
  let parameter_type = snd lam_var in
  let lambda_stack = [ `Value ] in
  let { Config.stack = _; instructions } =
    seq [ Slot.let_ parameter_slot ~in_:return ] lambda_stack
  in
  Config.ok (`Value :: stack) [ I.lambda parameter_type return_type instructions ]

(* https://tezos.gitlab.io/michelson-reference/#instr-LAMBDA_REC *)
let lambda_rec ~lam_var ~mu ~return_type return stack =
  let parameter_slot =
    match lam_var with
    | ident, _ -> `Ident ident
  in
  let parameter_type =
    match lam_var with
    | _, param_type -> param_type
  in
  let lambda_stack = [ `Value; `Value ] in
  let { Config.stack = _; instructions } =
    seq [ Slot.let_all [ parameter_slot; `Ident mu ] ~in_:return ] lambda_stack
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
    Config.ok stack [ I.create_contract storage parameter (code (`Value :: stack)) ]
  | _ ->
    raise_s
      [%message "Instruction.create_contract: invalid stack" (stack : SlotStack.t)]

let is_nat = prim 1 1 I.is_nat (* https://tezos.gitlab.io/michelson-reference/#instr-ISNAT *)
let unit = prim 0 1 I.unit (* https://tezos.gitlab.io/michelson-reference/#instr-UNIT *)
let push type_ lit = prim 0 1 (I.push type_ lit) (* https://tezos.gitlab.io/michelson-reference/#instr-PUSH *)
let apply = prim 2 1 I.apply (* https://tezos.gitlab.io/michelson-reference/#instr-APPLY *)
let exec = prim 2 1 I.exec (* https://tezos.gitlab.io/michelson-reference/#instr-EXEC *)
let unpair = prim 1 2 I.unpair (* https://tezos.gitlab.io/michelson-reference/#instr-UNPAIR *)
let pack = prim 1 1 I.pack (* https://tezos.gitlab.io/michelson-reference/#instr-PACK *)
let some = prim 1 1 I.some (* https://tezos.gitlab.io/michelson-reference/#instr-SOME *)
let update = prim 3 1 I.update (* https://tezos.gitlab.io/michelson-reference/#instr-UPDATE *)
let add = prim 2 1 I.add (* https://tezos.gitlab.io/michelson-reference/#instr-ADD *)
let sub = prim 2 1 I.sub (* https://tezos.gitlab.io/michelson-reference/#instr-SUB *)
let mul = prim 2 1 I.mul (* https://tezos.gitlab.io/michelson-reference/#instr-MUL *)
let pair = prim 2 1 I.pair (* https://tezos.gitlab.io/michelson-reference/#instr-PAIR *)
let car = prim 1 1 I.car (* https://tezos.gitlab.io/michelson-reference/#instr-CAR *)
let cdr = prim 1 1 I.cdr (* https://tezos.gitlab.io/michelson-reference/#instr-CDR *)
let get = prim 2 1 I.get (* https://tezos.gitlab.io/michelson-reference/#instr-GET *)
let unpack type_ = prim 1 1 (I.unpack type_) (* https://tezos.gitlab.io/michelson-reference/#instr-UNPACK *)
let neg = prim 1 1 I.neg (* https://tezos.gitlab.io/michelson-reference/#instr-NEG *)
let ediv = prim 2 1 I.ediv (* https://tezos.gitlab.io/michelson-reference/#instr-EDIV *)

let failwithf fmt =
  Fmt.kstr (fun msg -> seq [ push T.string (M.string msg); failwith ]) fmt

let and_ = prim 2 1 I.and_ (* https://tezos.gitlab.io/michelson-reference/#instr-AND *)
let or_ = prim 2 1 I.or_ (* https://tezos.gitlab.io/michelson-reference/#instr-OR *)
let not = prim 1 1 I.not  (* https://tezos.gitlab.io/michelson-reference/#instr-NOT *)
let left type_ = prim 1 1 (I.left type_) (* https://tezos.gitlab.io/michelson-reference/#instr-LEFT *)
let right type_ = prim 1 1 (I.right type_) (* https://tezos.gitlab.io/michelson-reference/#instr-RIGHT *)
let empty_map key_type val_type = prim 0 1 (I.empty_map key_type val_type) (* https://tezos.gitlab.io/michelson-reference/#instr-EMPTY_MAP *)
let compare = prim 2 1 I.compare (* https://tezos.gitlab.io/michelson-reference/#instr-COMPARE *)
let eq = prim 1 1 I.eq (* https://tezos.gitlab.io/michelson-reference/#instr-EQ *)
let neq = prim 1 1 I.neq (* https://tezos.gitlab.io/michelson-reference/#instr-NEQ *)
let lt = prim 1 1 I.lt (* https://tezos.gitlab.io/michelson-reference/#instr-LT *)
let le = prim 1 1 I.le (* https://tezos.gitlab.io/michelson-reference/#instr-LE *)
let gt = prim 1 1 I.gt (* https://tezos.gitlab.io/michelson-reference/#instr-GT *)
let ge = prim 1 1 I.ge (* https://tezos.gitlab.io/michelson-reference/#instr-GE *)
let int = prim 1 1 I.int (* https://tezos.gitlab.io/michelson-reference/#instr-INT *)
let debug = ref true
let next_trace_point = ref (-1)

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

let trace t =
  let trace_point =
    incr next_trace_point;
    !next_trace_point
  in
  seq
    [ (fun stack ->
        print_s [%message "Stack before" (trace_point : int) (stack : SlotStack.t)] stack)
    ; t
    ; (fun stack ->
        print_s [%message "Stack after" (trace_point : int) (stack : SlotStack.t)] stack)
    ]
