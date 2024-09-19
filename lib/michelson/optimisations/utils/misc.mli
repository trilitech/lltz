(* Copyright 2022-2023 Morum LLC, 2019-2022 Smart Chain Arena LLC *)

val memoize : ?clear_after:int -> (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b

val with_buffer : (Buffer.t -> unit) -> string

val buffer_protect :
  Buffer.t -> bool -> string -> string -> (unit -> unit) -> unit

val buffer_concat : Buffer.t -> string -> ('a -> unit) -> 'a list -> unit
