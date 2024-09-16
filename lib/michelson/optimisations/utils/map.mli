(* Copyright 2022-2023 Morum LLC, 2019-2022 Smart Chain Arena LLC *)

module type S = sig
  include Stdlib.Map.S

  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit

  val lookup : default:'a -> key -> 'a t -> 'a

  val to_list : 'a t -> (key * 'a) list

  val keys : 'a t -> key list

  val values : 'a t -> 'a list

  val of_list : (key * 'a) list -> 'a t

  val of_hashtbl : (key, 'a) Hashtbl.t -> 'a t

  val intersect : 'a t -> 'b t -> ('a * 'b) t
end

module type OrderedType = sig
  include Stdlib.Map.OrderedType

  val pp : Format.formatter -> t -> unit
end

module Make (Ord : OrderedType) : S with type key = Ord.t
