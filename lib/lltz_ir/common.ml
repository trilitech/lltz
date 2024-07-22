open Core

type location =
  { source : string
  ; start : int
  ; length : int
  }
[@@deriving sexp, equal, compare]

type label = Label of string [@@ocaml.unboxed] [@@deriving sexp, equal, compare]
