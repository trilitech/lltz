module I = Michelson.Ast.Instruction

module ExtStack = struct
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
  { stack : ExtStack.t
  ; instructions : I.t list
  }

let raise instructions = { stack = Exceptional; instructions }
let ok stack instructions = { stack = Ok stack; instructions }
let instructions t = t.instructions

(* merge two configs (e.g. representing different branches of an if) *)
let merge t1 t2 ~f =
  { stack = ExtStack.merge t1.stack t2.stack
  ; instructions = f t1.instructions t2.instructions
  }
;;
