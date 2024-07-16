type protocol =
  | Mumbai
  | Nairobi
  | Oxford
[@@deriving eq, ord, show {with_path = false}, sexp, yojson]
