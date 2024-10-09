(* Copyright 2022-2023 Morum LLC, 2019-2022 Smart Chain Arena LLC *)
type t = Big_int.big_int [@@deriving eq, ord, show]

val of_int : int -> t

val of_string : ?msg:string -> string -> t

include module type of Big_int with type big_int := t
