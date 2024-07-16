(* Copyright 2022-2023 Morum LLC, 2019-2022 Smart Chain Arena LLC *)

include module type of Stdlib.Int

include Data.S with type t = int

val pp : Format.formatter -> t -> unit
