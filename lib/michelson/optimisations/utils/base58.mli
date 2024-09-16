type t = [ `Base58 of string ]

val of_bytes : string -> t

val to_bytes_exn : t -> string
