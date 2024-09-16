(* Copyright 2022-2023 Morum LLC, 2019-2022 Smart Chain Arena LLC *)

include module type of Stdlib.String

val take : int -> string -> string

val drop : int -> string -> string

val is_prefix : string -> string -> bool

val pp : Format.formatter -> t -> unit

val repeat : string -> int -> string

val explode : string -> char list

val implode : char list -> string

include Data.S with type t = string
