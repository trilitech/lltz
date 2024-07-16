(* Copyright 2022-2023 Morum LLC, 2019-2022 Smart Chain Arena LLC *)

module type JsonGetter = sig
  val null : bool

  val has_member : string -> bool

  val get : string -> Yojson.Safe.t

  val string : string -> string

  val string_option : string -> string option

  val string_list : string -> string list

  val int : string -> int

  val bool : string -> bool
end

val json_getter : Yojson.Safe.t -> (module JsonGetter)

val json_sub : (module JsonGetter) -> string -> (module JsonGetter)

val ( <|> ) : 'a option -> (unit -> 'a option) -> 'a option

val memoize : ?clear_after:int -> (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b

val pp_with_max_indent :
  Format.formatter -> int -> (unit -> unit) -> unit -> unit

val pp_with_margin : Format.formatter -> int -> (unit -> unit) -> unit -> unit

type json =
  | J_int of Bigint.t
  | J_bool of bool
  | J_string of string
  | J_list of json list
  | J_record of (string * json) list
[@@deriving eq, ord, show]

val json_to_json : json -> Yojson.Safe.t

val pp_json_as_json :
  ?margin:int -> ?max_indent:int -> unit -> Format.formatter -> json -> unit

val tic : unit -> string -> unit

val tictoc : string -> ('a -> 'b) -> 'a -> 'b

val with_buffer : (Buffer.t -> unit) -> string

val buffer_protect :
  Buffer.t -> bool -> string -> string -> (unit -> unit) -> unit

val buffer_concat : Buffer.t -> string -> ('a -> unit) -> 'a list -> unit

val tick : string -> unit

val mkdir_p : ?perm:int -> string -> unit

type yojson_raw = Yojson.Safe.t

val yojson_raw_of_yojson : yojson_raw -> yojson_raw

val yojson_of_yojson_raw : yojson_raw -> yojson_raw
