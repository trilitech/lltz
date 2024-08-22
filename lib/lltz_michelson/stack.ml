open Core

type t = Slot.t list [@@deriving equal, compare, sexp]

let empty = []

let find t slot =
  List.findi t ~f:(fun _i slot' -> Slot.equal (slot :> Slot.t) slot')
  |> Option.map ~f:fst

let find_exn t slot =
  (*debug output*)
  match find t slot with
  | Some idx -> idx
  | None ->
    raise_s
      [%message
        "Instruction.Stack.find_exn: slot not found"
          (t : t)
          (slot : Slot.definable)]

  (* merge two stacks, if a slot is `Ident in both stacks, it must be the same ident *)
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