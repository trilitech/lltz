(* Copyright 2022-2023 Morum LLC, 2019-2022 Smart Chain Arena LLC *)

val id : 'a -> 'a

val pair : 'a -> 'b -> 'a * 'b

val flip : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c

val map_fst : ('a -> 'b) -> 'a * 'c -> 'b * 'c

val map_snd : ('a -> 'b) -> 'c * 'a -> 'c * 'b

module type TYPE = sig
  type t
end

module type EQ = sig
  type t

  val equal : t -> t -> bool
end

module type ORD = sig
  type t

  val compare : t -> t -> int
end

module type SHOW = sig
  type t

  val show : t -> string

  val pp : Format.formatter -> t -> unit
end

module type TYPE1 = sig
  type 'a t
end

module type EQ1 = sig
  type 'a t

  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
end

module type ORD1 = sig
  type 'a t

  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
end

module type SHOW1 = sig
  type 'a t

  val show : (Format.formatter -> 'a -> unit) -> 'a t -> string

  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
end

module type FUNCTOR_CORE = sig
  include TYPE1

  val map : ('a -> 'b) -> 'a t -> 'b t
end

module type APPLICATIVE_CORE = sig
  include FUNCTOR_CORE

  val return : 'a -> 'a t

  val apply : ('a -> 'b) t -> 'a t -> 'b t
end

module type MONAD_CORE = sig
  include APPLICATIVE_CORE

  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

module type APPLICATIVE_SYNTAX = sig
  type 'a t

  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t

  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
end

module type MONAD_SYNTAX = sig
  type 'a t

  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

  val ( and* ) : 'a t -> 'b t -> ('a * 'b) t
end

module type FUNCTOR = sig
  include FUNCTOR_CORE

  val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t
end

module type APPLICATIVE = sig
  include APPLICATIVE_CORE

  include FUNCTOR with type 'a t := 'a t

  include APPLICATIVE_SYNTAX with type 'a t := 'a t

  module Applicative_syntax : APPLICATIVE_SYNTAX with type 'a t := 'a t

  val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t

  val void : 'a t -> unit t

  val when_ : bool -> unit t -> unit t

  val when_some : 'a option -> ('a -> unit t) -> unit t

  val unless : bool -> unit t -> unit t

  val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t

  val map_option : ('a -> 'b t) -> 'a option -> 'b option t

  val map_list : ('a -> 'b t) -> 'a list -> 'b list t

  val iter_list : ('a -> unit t) -> 'a list -> unit t

  val map2_list : ('a -> 'b -> 'c t) -> 'a list -> 'b list -> 'c list t

  val iter2_list : ('a -> 'b -> unit t) -> 'a list -> 'b list -> unit t

  val sequence_list : 'a t list -> 'a list t

  val sequence_option : 'a t option -> 'a option t

  val sequence_fst : 'a t * 'b -> ('a * 'b) t

  val sequence_snd : 'a * 'b t -> ('a * 'b) t

  val sequence_pair : 'a t * 'b t -> ('a * 'b) t

  val mapM_fst : ('a -> 'c t) -> 'a * 'b -> ('c * 'b) t

  val mapM_snd : ('b -> 'c t) -> 'a * 'b -> ('a * 'c) t
end

module type MONAD = sig
  include MONAD_CORE

  include APPLICATIVE with type 'a t := 'a t

  include MONAD_SYNTAX with type 'a t := 'a t

  module Monad_syntax : MONAD_SYNTAX with type 'a t := 'a t

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

  val ( >> ) : unit t -> 'a t -> 'a t

  val join : 'a t t -> 'a t

  val bind2 : ('a -> 'b -> 'c t) -> 'a t -> 'b t -> 'c t

  val unless : bool -> unit t -> unit t
end

module Functor (A : FUNCTOR_CORE) : FUNCTOR with type 'a t := 'a A.t

module Applicative (A : APPLICATIVE_CORE) : APPLICATIVE with type 'a t := 'a A.t

module Monad (A : MONAD_CORE) : MONAD with type 'a t := 'a A.t

module type FIXPOINT = sig
  type 'a f

  type t = F of t f

  val unF : t -> t f
end

module Result : sig
  type 'a t = ('a, string) result [@@deriving eq, ord, show]

  val ok : 'a -> 'a t

  val error : string -> 'a t

  val cata : ('a -> 'b) -> (string -> 'b) -> 'a t -> 'b

  include MONAD with type 'a t := 'a t

  val get_ok : 'a t -> 'a option

  val get_ok_exn : 'a t -> 'a

  val is_ok : _ t -> bool

  val is_error : _ t -> bool

  val map_error : (string -> string) -> 'a t -> 'a t

  val get_error : 'a t -> string option
end

val curry : ('a * 'b -> 'c) -> 'a -> 'b -> 'c

val uncurry : ('a -> 'b -> 'c) -> 'a * 'b -> 'c