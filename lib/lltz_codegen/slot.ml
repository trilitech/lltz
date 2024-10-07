type definable =
  [ `Heap
  | `Ident of Ident.t
  ]
[@@deriving equal, compare, sexp]

type t =
  [ `Value
  | definable
  ]
[@@deriving equal, compare, sexp]

let is_assignable (from : t) ~(to_ : [< definable ]) =
  match from, to_ with
  | `Heap, `Heap -> true
  | `Heap, `Ident (_ : string) -> false
  | `Value, _ -> true
  | `Ident _, _ -> false
;;
