(* This file defines modules used for creating sequences of Michelson instructions during LLTZ compilation. It defines
   Stack modules that specifies current types of slots (variable identificator or value, heap unused in LLTZ).
   Each instruction defined in this file takes a stack and returns a new Config with updated stack and instructions.
   The Config module is used to store the current stack and instructions and handling exeception cases where shape of the stack
   does not match instruction requirements.
*)

include Core

module M = Michelson.Ast 
module I = M.Instruction
module T = M.Type

module Ident = struct
  include String

  let create =
    let next = ref 0 in
    fun ?(prefix = "_") () ->
      Int.incr next;
      prefix ^ Int.to_string !next
  ;;
end

module Stack = struct
  module Slot = struct
    type definable =
      [ `Heap
      | `Ident of Ident.t
      ]
    [@@deriving equal, compare, sexp]

    type t =
      [ `Value
      | definable
      ]
    [@@deriving equal, compare, sexp]

    let is_assignable (from : t) ~(to_ : [< definable ]) =
      match from, to_ with
      | `Heap, `Heap -> true
      | `Heap, `Ident (_ : string) -> false
      | `Value, _ -> true
      | `Ident _, _ -> false
    ;;
  end

  type t = Slot.t list [@@deriving equal, compare, sexp]

  let empty = []

  let find t slot =
    List.findi t ~f:(fun _i slot' -> Slot.equal (slot :> Slot.t) slot')
    |> Option.map ~f:fst
  ;;

  let find_exn t slot =
    (*debug output*)

    match find t slot with
    | Some idx -> idx
    | None ->
      raise_s
        [%message
          "Instruction.Stack.find_exn: slot not found"
            (t : t)
            (slot : [ `Heap | `Ident of Ident.t ])]
  ;;

  let merge t1 t2 : t =
    match
      List.map2 t1 t2 ~f:(fun slot1 slot2 ->
          match slot1, slot2 with
          | `Ident x, `Ident y when Ident.(x = y) -> `Ident x
          | `Heap, `Heap -> `Heap
          | _ -> `Value)
    with
    | Ok t -> t
    | Unequal_lengths ->
      raise_s
        [%message "Stack.join: cannot join stacks of unequal size" (t1 : t) (t2 : t)]
  ;;
end

module Config = struct
  module Stack = struct
    type t =
      | Ok of Stack.t
      | Exceptional
    [@@deriving equal, compare, sexp]

    let apply t ~f =
      match t with
      | Ok stack -> Ok (f stack)
      | Exceptional -> Exceptional
    ;;

    let merge t1 t2 =
      match t1, t2 with
      | Ok stack1, Ok stack2 -> Ok (Stack.merge stack1 stack2)
      | Exceptional, stack | stack, Exceptional -> stack
    ;;
  end

  type t =
    { stack : Stack.t
    ; instructions : I.t list
    }

  let raise instructions = { stack = Exceptional; instructions }
  let ok stack instructions = { stack = Ok stack; instructions }
  let instructions t = t.instructions

  let merge t1 t2 ~f =
    { stack = Stack.merge t1.stack t2.stack
    ; instructions = f t1.instructions t2.instructions
    }
  ;;
end

type t = Stack.t -> Config.t

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
;;

let dig n stack =
  (* brings element at index n to tos *)
  let stack =
    match List.split_n stack n with
    | slots1, nth :: slots2 -> (nth :: slots1) @ slots2
    | _ -> raise_s [%message "Instruction.dig: invalid stack" (stack : Stack.t) (n : int)]
  in
  Config.ok stack
  @@
  match n with
  | 0 -> []
  | 1 -> [ I.swap ]
  | n -> [ I.dig_n n ]
;;

let dug n stack =
  (* n is a index into the stack, tos will be at index n after instr *)
  let stack =
    match List.split_n stack (n + 1) with
    | nth :: slots1, slots2 -> slots1 @ (nth :: slots2)
    | _ -> raise_s [%message "Instruction.dig: invalid stack" (stack : Stack.t) (n : int)]
  in
  Config.ok stack
  @@
  match n with
  | 0 -> []
  | 1 -> [ I.swap ]
  | n -> [ I.dug_n n ]
;;

let dup n stack =
  (* n is a index into the stack *)
  let stack =
    match List.split_n stack n with
    | left, nth :: right -> (`Value :: left) @ (nth :: right)
    | _ -> raise_s [%message "Instruction.dup: invalid stack" (stack : Stack.t) (n : int)]
  in
  Config.ok stack [ I.dup_n (n + 1) ]
;;

let swap stack =
  let stack =
    match stack with
    | s0 :: s1 :: slots -> s1 :: s0 :: slots
    | _ -> raise_s [%message "Instruction.swap: invalid stack" (stack : Stack.t)]
  in
  Config.ok stack [ I.swap ]
;;

let drop n stack =
  (* drop's n elements (n is a length) *)
  let stack = List.drop stack n in
  Config.ok stack [ I.drop_n n ]
;;

let remove n stack =
  (* removes element at index n *)
  let stack =
    match List.split_n stack n with
    | left, _nth :: right -> left @ right
    | _ ->
      raise_s [%message "Instruction.remove: invalid stack" (stack : Stack.t) (n : int)]
  in
  Config.ok stack
  @@
  match n with
  | 0 -> [ I.drop ]
  | 1 -> I.[ swap; drop ]
  | n -> I.[ dig_n n; drop ]
;;

let prim m n instr stack =
  let stack =
    let left, right = List.split_n stack m in
    if not
         (List.for_all left ~f:(function
             | `Value -> true
             | _ -> false))
    then
      raise_s
        [%message "Instruction.prim: invalid stack" (stack : Stack.t) (m : int) (n : int)];
    List.init n ~f:(fun _ -> `Value) @ right
  in
  Config.ok stack [ instr ]
;;

let noop stack = Config.ok stack []

let loop (in_ : t) stack =
  let stack =
    match stack with
    | `Value :: stack -> stack
    | _ -> raise_s [%message "Instruction.loop" (stack : Stack.t)]
  in
  let instrs = Config.instructions @@ in_ stack in
  Config.ok stack [ I.loop instrs ]
;;

let loop_left (in_ : t) stack =
  let stack =
    match stack with
    | `Value :: stack -> `Value :: stack
    | _ -> raise_s [%message "Instruction.loop_left" (stack : Stack.t)]
  in
  let instrs = Config.instructions @@ in_ stack in
  Config.ok stack [ I.loop_left instrs ]

let iter in_ stack =
  let stack =
    match stack with
    | `Value :: slots -> slots
    | _ -> raise_s [%message "Instruction.iter" (stack : Stack.t)]
  in
  let instrs = Config.instructions @@ in_ (`Value :: stack) in
  Config.ok stack [ I.iter instrs ]
;;

let map_ in_ stack =
  let stack =
    match stack with
    | `Value :: slots -> slots
    | _ -> raise_s [%message "Instruction.map" (stack : Stack.t)]
  in
  let instrs = Config.instructions @@ in_ (`Value :: stack) in
  Config.ok stack [ I.map instrs ]
;;

let if_ ~then_ ~else_ stack =
  let stack =
    match stack with
    | `Value :: slots -> slots
    | _ -> raise_s [%message "Instruction.if_" (stack : Stack.t)]
  in
  Config.merge (then_ stack) (else_ stack) ~f:(fun instrs1 instrs2 ->
      [ I.if_ ~then_:instrs1 ~else_:instrs2 ])
;;

let if_left ~left ~right stack =
  let stack =
    match stack with
    | `Value :: slots -> `Value :: slots
    | _ -> raise_s [%message "Instruction.if_left" (stack : Stack.t)]
  in
  Config.merge (left stack) (right stack) ~f:(fun instrs1 instrs2 ->
      [ I.if_left ~then_:instrs1 ~else_:instrs2 ])
;;

let if_cons ~empty ~nonempty stack =
  let stack =
    match stack with
    | `Value :: slots -> slots
    | _ -> raise_s [%message "Instruction.if_cons" (stack : Stack.t)]
  in
  Config.merge (empty stack) (nonempty (`Value :: `Value :: stack)) ~f:(fun instrs1 instrs2 ->
      [ I.if_cons ~then_:instrs1 ~else_:instrs2 ])

let if_none ~none ~some stack =
  let stack =
    match stack with
    | `Value :: slots -> slots
    | _ -> raise_s [%message "Instruction.if_none" (stack : Stack.t)]
  in
  Config.merge
    (none stack)
    (some (`Value :: stack))
    ~f:(fun instrs1 instrs2 -> [ I.if_none ~then_:instrs1 ~else_:instrs2 ])
;;

module Slot = struct
  let def (slot : [< Stack.Slot.definable ]) ~in_ stack =
    let stack =
      match stack with
      | slot' :: stack ->
        if not (Stack.Slot.is_assignable slot' ~to_:slot)
        then raise_s [%message "Instruction.Slot.def: slot not assignable"];
        (slot :> Stack.Slot.t) :: stack
      | _ ->
        raise_s
          [%message
            "Instruction.Slot.bind: invalid stack"
              (stack : Stack.t)
              (slot : [< Stack.Slot.definable ])]
    in
    in_ stack
  ;;

  let collect slot stack = remove (Stack.find_exn stack slot) stack
  let let_ slot ~in_ = seq [ def slot ~in_; collect slot ]

  let def_all slots' ~in_ stack =
    let stack =
      let check_slots slots =
        if not
             (List.for_all slots ~f:(fun (slot, slot') ->
                  Stack.Slot.is_assignable slot ~to_:slot'))
        then
          raise_s
            [%message
              "Instruction.Slot.def_all: invalid slots"
                (stack : Stack.t)
                (slots' : [< Stack.Slot.definable ] list)]
      in
      match List.zip_with_remainder stack slots' with
      | slots, None ->
        check_slots slots;
        let slots = (slots :> (Stack.Slot.t * Stack.Slot.t) list) in
        List.map ~f:snd slots
      | slots, Some (First right) ->
        check_slots slots;
        let slots = (slots :> (Stack.Slot.t * Stack.Slot.t) list) in
        List.map ~f:snd slots @ right
      | _ ->
        raise_s [%message "Instruction.Slot.def_all: invalid stack" (stack : Stack.t)]
    in
    in_ stack
  ;;

  let collect_all slots = seq (List.map slots ~f:collect)
  let let_all slots ~in_ = seq [ def_all slots ~in_; collect_all slots ]

  let lookup slot ~in_ stack =
    let idx = Stack.find_exn stack slot in
    in_ idx stack
  ;;

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
                (stack : Stack.t)
                (slot : [< Stack.Slot.definable ])])
  ;;
end

let dip n (t : t) : t =
 fun stack ->
  let left_stack, right_stack = List.split_n stack n in
  let { Config.stack = right_stack; instructions } = t right_stack in
  let stack =
    Config.Stack.apply right_stack ~f:(fun right_stack -> left_stack @ right_stack)
  in
  { Config.stack; instructions = [ I.dip_n n instructions ] }
;;

let unpair_n n =
  (* invariant: pop 1 value, push n *)
  match n with
  | 0 -> drop 1
  | 1 -> noop
  | n -> prim 1 n (I.unpair_n n)
;;

let pair_n n =
  (* invariant: pop n values, push 1 *)
  match n with
  | 0 -> prim 0 1 I.unit
  | 1 -> noop
  | n -> prim n 1 (I.pair_n n)
;;

let get_n idx ~length:n =
  match n with
  | 0 ->
    raise_s [%message "Comp.Instruction.tuple_project: cannot project from empty tuple"]
  | 1 ->
    assert (idx = 0);
    noop
  | n ->
    (* n >= 2 *)
    let k = if idx = n - 1 then 2 * idx else (2 * idx) + 1 in
    prim 1 1 (I.get_n k)
;;

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
;;

(*let lambda ~closure ~parameters ~return_type return stack =
  let n = List.length closure + List.length parameters + 1 in
  let closure_slots = List.map closure ~f:(fun (ident, _) -> `Ident ident) in
  let parameter_slots = List.map parameters ~f:(fun (ident, _) -> `Ident ident) in
  let return_pair stack =
    match stack with
    | [ `Value; `Heap ] -> Config.ok [ `Value ] I.[ swap; pair ]
    | _ -> raise_s [%message "Instruction.lambda: invalid stack" (stack : Stack.t)]
  in
  let lambda_stack = [ `Value ] in
  let { Config.stack = _; instructions } =
    seq
      [ unpair_n n
      ; Slot.def_all (closure_slots @ (`Heap :: parameter_slots)) ~in_:return
      ; Slot.collect_all (closure_slots @ parameter_slots)
      ; return_pair
      ]
      lambda_stack
  in
  let parameter_type =
    Type.tuple (List.map closure ~f:snd @ [ Type.heap ] @ List.map parameters ~f:snd)
  in
  let return_type = Type.(tuple [ heap; return_type ]) in
  Config.ok (`Value :: stack) [ I.lambda parameter_type return_type instructions ]
;;*)

(*No need for heap in lambda*)
let lambda  ~lam_var ~return_type return stack =
  let parameter_slot = (match lam_var with
    | (ident, _) -> (`Ident ident)) in
  let parameter_type =(match lam_var with
  | (_, param_type) -> param_type) in
  let lambda_stack = [ `Value ] in
  let { Config.stack = _; instructions } =
    seq
      [ 
         Slot.let_ ( parameter_slot) ~in_:return;
      ]
      lambda_stack
  in
  Config.ok (`Value :: stack) [ I.lambda parameter_type return_type instructions ];;

let lambda_rec ~lam_var ~mu ~return_type return stack =
  let parameter_slot = (match lam_var with
    | (ident, _) -> (`Ident ident)) in
  let parameter_type =(match lam_var with
  | (_, param_type) -> param_type) in
  let lambda_stack = [ `Value; `Value ] in
  let { Config.stack = _; instructions } =
    seq
      [ 
         Slot.let_all [ parameter_slot; `Ident mu ] ~in_:return;
      ]
      lambda_stack
  in
  Config.ok (`Value :: stack) [ I.lambda_rec parameter_type return_type instructions ];;

let never stack =
  (match stack with
  | `Value :: _ -> ()
  | _ -> raise_s [%message "Instruction.never: invalid stack" (stack : Stack.t)]);
  Config.raise [ I.never ]
;;

let failwith stack =
  (match stack with
  | `Value :: _ -> ()
  | _ -> raise_s [%message "Instruction.failwith: invalid stack" (stack : Stack.t)]);
  Config.raise [ I.failwith ]
;;

let create_contract ~storage ~parameter ~code stack =
  let stack =
    match stack with
    | `Value :: `Value :: `Value :: stack -> `Value :: stack
    | _ -> raise_s [%message "Instruction.create_contract: invalid stack" (stack : Stack.t)]
  in
  Config.ok stack [ I.create_contract storage parameter (code stack) ]

let is_nat = prim 1 1 I.is_nat
let unit = prim 0 1 I.unit
let push type_ lit = prim 0 1 (I.push type_ lit)
let apply = prim 2 1 I.apply
let exec = prim 2 1 I.exec
let unpair = prim 1 2 I.unpair
let pack = prim 1 1 I.pack
let some = prim 1 1 I.some
let update = prim 3 1 I.update
let add = prim 2 1 I.add
let sub = prim 2 1 I.sub
let mul = prim 2 1 I.mul
let pair = prim 2 1 I.pair
let car = prim 1 1 I.car
let cdr = prim 1 1 I.cdr
let get = prim 2 1 I.get
let unpack type_ = prim 1 1 (I.unpack type_)
let neg = prim 1 1 I.neg
let ediv = prim 2 1 I.ediv

let failwithf fmt =
  Fmt.kstr (fun msg -> seq [ push T.string (M.string msg); failwith ]) fmt
;;

let and_ = prim 2 1 I.and_
let or_ = prim 2 1 I.or_
let not = prim 1 1 I.not
let left type_ = prim 1 1 (I.left type_)
let right type_ = prim 1 1 (I.right type_)
let empty_map key_type val_type = prim 0 1 (I.empty_map key_type val_type)
let compare = prim 2 1 I.compare
let eq = prim 1 1 I.eq
let neq = prim 1 1 I.neq
let lt = prim 1 1 I.lt
let le = prim 1 1 I.le
let gt = prim 1 1 I.gt
let ge = prim 1 1 I.ge
let int = prim 1 1 I.int
let debug = ref true
let next_trace_point = ref (-1)



let set_debug next in_ =
  let curr = !debug in
  debug := next;
  let result = in_ () in
  debug := curr;
  result
;;

let to_michelson t stack ~debug =
  set_debug debug (fun () ->
      let Config.{ stack = _; instructions } = t stack in
      instructions)
;;

let print_s sexp stack =
  if !debug then print_s sexp;
  Config.ok stack []
;;

let trace t =
  let trace_point =
    incr next_trace_point;
    !next_trace_point
  in
  seq
    [ (fun stack ->
        print_s [%message "Stack before" (trace_point : int) (stack : Stack.t)] stack)
    ; t
    ; (fun stack ->
        print_s [%message "Stack after" (trace_point : int) (stack : Stack.t)] stack)
    ]
;;