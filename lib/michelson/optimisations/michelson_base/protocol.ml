type protocol =
  | Nairobi
  | Oxford
  | Paris
[@@deriving eq, ord, show {with_path = false}, sexp, yojson]
