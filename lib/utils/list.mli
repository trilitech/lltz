(* Copyright 2022-2023 Morum LLC, 2019-2022 Smart Chain Arena LLC *)

include module type of Stdlib.List with type 'a t := 'a list

type 'a t = 'a list [@@deriving eq, ord, show]

val rnth : 'a list -> int -> 'a

val rnth_opt : 'a list -> int -> 'a option

val nth_rem : int -> 'a list -> ('a * 'a list) option

val last : 'a list -> 'a

val last_opt : 'a list -> 'a option

val split_at_opt : int -> 'a list -> ('a list * 'a list) option

val split_at : ?err:string -> int -> 'a list -> 'a list * 'a list

val take : int -> 'a list -> 'a list

val drop : int -> 'a list -> 'a list

val rdrop : int -> 'a list -> 'a list

val rtl : 'a list -> 'a list

val unsnoc : 'a list -> 'a list * 'a

val map3 : ?err:string -> ('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t

val find_ix : 'a -> 'a t -> int option

val is_prefix : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool

val is_suffix : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool

val strip_common_prefix : ('a -> 'b -> bool) -> 'a t -> 'b t -> 'a t * 'b t

val strip_common_suffix : ('a -> 'b -> bool) -> 'a t -> 'b t -> 'a t * 'b t

val map2 : ?err:string -> ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t

val map_some : ('a -> 'b option) -> 'a t -> 'b t

val find_some : ('a -> 'b option) -> 'a t -> 'b option

val somes : 'a option t -> 'a t

val intersperse : 'a -> 'a list -> 'a list

val assoc_opt : ?equal:('a -> 'a -> bool) -> 'a -> ('a * 'b) t -> 'b option

val remove_assoc : ?equal:('a -> 'a -> bool) -> 'a -> ('a * 'b) t -> ('a * 'b) t

val assoc_exn : ?msg:string -> 'a -> ('a * 'b) t -> 'b

val replicate : int -> 'a -> 'a list

val pp_sep :
  string -> (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit

val buffer_sep :
  (Buffer.t -> unit) -> (Buffer.t -> 'a -> unit) -> Buffer.t -> 'a t -> unit
