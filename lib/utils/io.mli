(* Copyright 2022-2023 Morum LLC, 2019-2022 Smart Chain Arena LLC *)

val read_file : string -> string

val write_file : string -> string -> unit

val with_out : string -> (out_channel -> 'a) -> 'a
