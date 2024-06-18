open Core

type label = Label of string [@@ocaml.unboxed] [@@deriving sexp, equal, compare]
