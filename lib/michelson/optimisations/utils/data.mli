(* Copyright 2022-2023 Morum LLC, 2019-2022 Smart Chain Arena LLC *)
open Sexplib0

module type KeyType = sig
  include Stdlib.Map.OrderedType

  val pp : Format.formatter -> t -> unit

  val t_of_sexp : Sexp.t -> t

  val sexp_of_t : t -> Sexp.t
end

module type S = sig
  include Stdlib.Set.OrderedType

  module Set : Set.S with type elt = t

  module Map : sig
    include Map.S with type key = t

    val t_of_sexp : (Sexp.t -> 'a) -> Sexp.t -> 'a t

    val sexp_of_t : ('a -> Sexp.t) -> 'a t -> Sexp.t

    val fold_with_key : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

    val fold : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
  end

  val map_of_set : (Map.key -> 'a) -> Set.t -> 'a Map.t
end

module Make (Ord : KeyType) : S with type t = Ord.t
