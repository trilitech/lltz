open Import
open Grace

type micheline = (Range.t, string) Tezos_micheline.Micheline.node

module T = struct
  type var = Var of string
  and mut_var = Mut_var of string

  and constant =
    | Unit
    | Bool of bool
    | Nat of (Z.t[@sexp.opaque])
    | Int of (Z.t[@sexp.opaque])
    | Mutez of (Z.t[@sexp.opaque])
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

  and t =
    { desc : desc
    ; range : Range.t
    ; type_ : Type.t
    }

  and binder = var * Type.t

  and lambda =
    { lam_var : var
    ; body : t
    }

  and lambda_typed =
    { lam_var : binder
    ; body : t
    }

  and lambda2 =
    { lam_var1 : var
    ; lam_var2 : var
    ; body : t
    }

  and let_in =
    { let_var : var
    ; rhs : t
    ; in_ : t
    }

  and lambda_rec =
    { mu_var : binder
    ; lambda : lambda_typed
    }

  and app =
    { abs : t
    ; arg : t
    }

  and let_mut_in =
    { let_var : mut_var
    ; rhs : t
    ; in_ : t
    }

  and if_bool =
    { condition : t
    ; if_true : t
    ; if_false : t
    }

  and if_none =
    { subject : t
    ; if_none : t
    ; if_some : lambda
    }

  and if_cons =
    { subject : t
    ; if_empty : t
    ; if_nonempty : lambda2
    }

  and if_left =
    { subject : t
    ; if_left : lambda
    ; if_right : lambda
    }

  and while_ =
    { cond : t
    ; body : t
    }

  and while_left =
    { cond : t
    ; body : lambda
    }

  and for_ =
    { index : mut_var
    ; init : t
    ; cond : t
    ; update : t
    ; body : t
    }

  and for_each =
    { collection : t
    ; body : lambda
    }

  and map_ =
    { collection : t
    ; map : lambda
    }

  and fold_left =
    { collection : t
    ; init : t
    ; fold : lambda
    }

  and fold_right =
    { collection : t
    ; init : t
    ; fold : lambda
    }

  and let_tuple_in =
    { components : var list
    ; rhs : t
    ; in_ : t
    }

  and update =
    { tuple : t
    ; component : Row.Path.t
    ; update : t
    }

  and raw_michelson =
    { michelson : (micheline[@sexp.opaque] [@equal.ignore] [@compare.ignore])
    ; args : t list
    }

  and global_constant = { hash : string }

  and create_contract =
    { storage : Type.t
    ; code : lambda_typed
    ; delegate : t
    ; initial_balance : t
    ; initial_storage : t
    }

  and desc =
    (* basic lambda calculus w/ primitives + constants *)
    | Variable of var
    | Let_in of let_in
    | Lambda of lambda_typed
    | Lambda_rec of lambda_rec
    | App of app
    | Const of constant
    | Prim of Primitive.t * t list (* mutability *)
    | Let_mut_in of let_mut_in
    | Deref of mut_var
    | Assign of mut_var * t
    (* low-level control flow (conditional) *)
    | If_bool of if_bool
    | If_none of if_none
    | If_cons of if_cons
    | If_left of if_left (* low-level control flow (iterative) *)
    | While of while_
    | While_left of while_left
    | For of for_
    | For_each of for_each (* high-level control flow (iterative) *)
    | Map of map_
    | Fold_left of fold_left
    | Fold_right of fold_right (* tuples *)
    | Let_tuple_in of let_tuple_in
    | Tuple of t Row.t
    | Proj of t * Row.Path.t
    | Update of update
    (* sums *)
    | Inj of Type.t Row.Context.t * t
    | Match of t * lambda_typed Row.t (* tezos specific *)
    | Raw_michelson of raw_michelson
    | Global_constant of global_constant
    | Create_contract of create_contract
  [@@deriving sexp, equal, compare, traverse]
end

include T

(*
module Traverse = struct
  class map =
    let zero = Traverse_builtins.map_zero in
    object
      inherit Traverse_builtins.map
      inherit Row.Traverse_builtins.map
      inherit T.map
      method z__t = zero
      method type__t = zero
      method row__path__t = zero
      method range__t = zero
      method primitive__t = zero
      method micheline = zero
    end

  class iter =
    let zero = Traverse_builtins.iter_zero in
    object
      inherit Traverse_builtins.iter
      inherit Row.Traverse_builtins.iter
      inherit T.iter
      method z__t = zero
      method type__t = zero
      method row__path__t = zero
      method range__t = zero
      method primitive__t = zero
      method micheline = zero
    end

  class ['acc] fold =
    let zero = Traverse_builtins.fold_zero in
    object
      inherit ['acc] Traverse_builtins.fold
      inherit ['acc] Row.Traverse_builtins.fold
      inherit ['acc] T.fold
      method z__t = zero
      method type__t = zero
      method row__path__t = zero
      method range__t = zero
      method primitive__t = zero
      method micheline = zero
    end

  class ['acc] fold_map =
    let zero = Traverse_builtins.fold_map_zero in
    object
      inherit ['acc] Traverse_builtins.fold_map
      inherit ['acc] Row.Traverse_builtins.fold_map
      inherit ['acc] T.fold_map
      method z__t = zero
      method type__t = zero
      method row__path__t = zero
      method range__t = zero
      method primitive__t = zero
      method micheline = zero
    end

  class ['ctx] map_with_context =
    let zero = Traverse_builtins.map_with_context_zero in
    object
      inherit ['ctx] Traverse_builtins.map_with_context
      inherit ['ctx] Row.Traverse_builtins.map_with_context
      inherit ['ctx] T.map_with_context
      method z__t = zero
      method type__t = zero
      method row__path__t = zero
      method range__t = zero
      method primitive__t = zero
      method micheline = zero
    end
end

*)
