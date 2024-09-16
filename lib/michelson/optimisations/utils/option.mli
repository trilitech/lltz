(* Copyright 2022-2023 Morum LLC, 2019-2022 Smart Chain Arena LLC *)
open Control

include module type of Stdlib.Option with type 'a t := 'a option

type 'a t = 'a option [@@deriving eq, show]

val cata : 'a -> ('b -> 'a) -> 'b t -> 'a

val some : 'a -> 'a t

include MONAD with type 'a t := 'a t

val default : 'a -> 'a t -> 'a

val is_none : 'a option -> bool

val is_some : 'a option -> bool

val of_some : ?msg:string -> 'a option -> 'a

val or_ : 'a t -> 'a t -> 'a t

val if_none : 'a t -> (unit -> 'a t) -> 'a t
