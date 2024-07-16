(* Copyright 2022-2023 Morum LLC, 2019-2022 Smart Chain Arena LLC *)

open Control

type 'a t =
  | Leaf of 'a
  | Node of 'a t * 'a t
[@@deriving eq, ord, show, fold, sexp]

val cata : ('a -> 'b) -> ('b -> 'b -> 'b) -> 'a t -> 'b

val size : 'a t -> int

include MONAD with type 'a t := 'a t

val leaf : 'a -> 'a t

val node : 'a t -> 'a t -> 'a t

val print :
     (Format.formatter -> unit)
  -> (Format.formatter -> 'a -> unit)
  -> Format.formatter
  -> 'a t
  -> unit

val to_list : 'a t -> 'a list

val fold1 : ('a -> 'a -> 'a) -> 'a t -> 'a

val map_accum : ('a -> 'b -> 'a * 'c) -> 'a -> 'b t -> 'a * 'c t

val right_comb : 'a list -> 'a t

val is_right_comb : 'a t -> bool

val map_some : ('a -> 'b option) -> 'a t -> 'b t option

val filter : ('a -> bool) -> 'a t -> 'a t option

val matches : 'a t -> 'b t -> ('a * 'b t) t option

val common : 'a t -> 'b t -> ('a t * 'b t) t

val common3 : 'a t -> 'b t -> 'c t -> ('a t * 'b t * 'c t) t

val same_shape : 'a t -> 'b t -> bool

val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t

val for_all : ('a -> bool) -> 'a t -> bool

val exists : ('a -> bool) -> 'a t -> bool

module Traversable (A : APPLICATIVE) : sig
  val map : ('a -> 'b A.t) -> 'a t -> 'b t A.t

  val sequence : 'a A.t t -> 'a t A.t
end

type 'a context =
  | Hole
  | Node_left of 'a context * 'a t
  | Node_right of 'a t * 'a context
[@@deriving eq, show, map]

val fill : 'a t -> 'a context -> 'a t

val find_leaf : ('a -> bool) -> 'a t -> ('a * 'a context) option

val context_cata :
  'b -> ('b -> 'a t -> 'b) -> ('a t -> 'b -> 'b) -> 'a context -> 'b

module Context : sig
  type 'a t = 'a context [@@deriving eq, show, map]
end
