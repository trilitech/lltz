(* Copyright 2022-2023 Morum LLC, 2019-2022 Smart Chain Arena LLC *)

open Sexplib0
open Sexplib.Std

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

module Make (Ord : KeyType) = struct
  include Ord
  module Set = Set.Make (Ord)

  module Map = struct
    include Map.Make (Ord)

    let key_of_sexp = Ord.t_of_sexp

    let sexp_of_key = Ord.sexp_of_t

    type 'a bindings = (key * 'a) list [@@deriving sexp]

    let sexp_of_t f x = sexp_of_bindings f @@ bindings x

    let t_of_sexp f x = of_list @@ bindings_of_sexp f x

    let fold_with_key = fold

    let fold f z m = fold (fun _ a b -> f b a) m z
  end

  let map_of_set f x = Map.of_seq (Seq.map (fun k -> (k, f k)) (Set.to_seq x))
end
