(* Copyright 2022-2023 Morum LLC, 2019-2022 Smart Chain Arena LLC *)

module type S = sig
  include Stdlib.Set.S

  val pp : Format.formatter -> t -> unit

  val unions : t list -> t

  val of_option : elt option -> t
end

module type OrderedType = sig
  include Stdlib.Map.OrderedType

  val pp : Format.formatter -> t -> unit
end

module Make (Ord : OrderedType) : S with type elt = Ord.t
