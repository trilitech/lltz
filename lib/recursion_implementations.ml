(*  Four different ways of implementing annotated expression trees *)
(*  Using an overly simplified expression type *)

(* Naive implementation with mutually recursive types *)
module Closed_recursion = struct
  type expr_base = Const of int | Add of expr * expr | Mul of expr * expr
  and expr = { deco : string; inner : expr_base }

  type t = expr

  (* Example code repeated 4 times *)
  let rec fib (n : int) : t =
    let deco = string_of_int n in
    let inner =
      match n with 0 | 1 -> Const 1 | n -> Add (fib (n - 2), fib (n - 1))
    in
    { deco; inner }

  let rec eval ({ inner; _ } : t) : int =
    match inner with
    | Const n -> n
    | Add (lhs, rhs) -> eval lhs + eval rhs
    | Mul (lhs, rhs) -> eval lhs * eval rhs
end

(* Break the mutual recursion with a parameterised decorator *)
(* This is almost identical to the mutually recursive version *)
module Open_recursion_annot = struct
  type 'a annotated = { deco : string; inner : 'a }

  type expr =
    | Const of int
    | Add of expr annotated * expr annotated
    | Mul of expr annotated * expr annotated

  type t = expr annotated

  (* Example code repeated 4 times *)
  let rec fib (n : int) : t =
    let deco = string_of_int n in
    let inner =
      match n with 0 | 1 -> Const 1 | n -> Add (fib (n - 2), fib (n - 1))
    in
    { deco; inner }

  let rec eval ({ inner; _ } : t) : int =
    match inner with
    | Const n -> n
    | Add (lhs, rhs) -> eval lhs + eval rhs
    | Mul (lhs, rhs) -> eval lhs * eval rhs
end

(* We can also break the recursion the other way *)
(* This seems less natural at first but we're doing the same thing *)
(* as above, just  breaking the sum type instead of the product type *)
module Open_recursion_node = struct
  type 'a expr_f = Const of int | Add of 'a * 'a | Mul of 'a * 'a
  type expr = { deco : string; inner : expr expr_f }
  type t = expr

  (* Example code repeated 4 times *)
  let rec fib (n : int) : t =
    let deco = string_of_int n in
    let inner =
      match n with 0 | 1 -> Const 1 | n -> Add (fib (n - 2), fib (n - 1))
    in
    { deco; inner }

  let rec eval ({ inner; _ } : t) : int =
    match inner with
    | Const n -> n
    | Add (lhs, rhs) -> eval lhs + eval rhs
    | Mul (lhs, rhs) -> eval lhs * eval rhs

  (* You can add fancy folds and unfolds here if you like or even mess about with ppx's *)
end

module Fancy_recursion = struct
  module Base = struct
    type 'a t = Const of int | Add of 'a * 'a | Mul of 'a * 'a
    [@@deriving map]
  end

  (* a little bit of messing about to make sure we get the same types *)
  (* as the other 3 implementations *)
  type expr_f = Base.t

  open Base
  include Recursion.Fixpoint (Base)

  type t = string Recursion.Fixpoint(Base).t

  (* Example code repeated 4 times *)
  (* still works out of the box dispite the fanciness *)
  let rec fib (n : int) : t =
    let deco = string_of_int n in
    let inner =
      match n with 0 | 1 -> Const 1 | n -> Add (fib (n - 2), fib (n - 1))
    in
    { deco; inner }

  let rec eval ({ inner; _ } : t) : int =
    match inner with
    | Const n -> n
    | Add (lhs, rhs) -> eval lhs + eval rhs
    | Mul (lhs, rhs) -> eval lhs * eval rhs

  (* Fancy versions if you like this sort of thing *)
  (* Completely optional so we don't need to do it if we don't want to *)

  (* unfolds can feel a bit unnatural *)
  (* I'm not convinced that this is better than the direct implementation *)
  (* but we don't have to do it *)
  let fancy_fib : int -> t =
    let expand n =
      ( string_of_int n
      , match n with 0 | 1 -> Const 1 | n -> Add (n - 2, n - 1) )
    in
    unfold expand

  (* folds for me work much better you get a clear separation of the *)
  (* evaluation logic and the recursion logic *)
  let fancy_eval : t -> int =
    let eval _ = function
      | Const n -> n
      | Add (a, b) -> a + b
      | Mul (a, b) -> a * b
    in
    fold eval
end
