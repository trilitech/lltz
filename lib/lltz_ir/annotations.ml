open Import

module StringSet = Set.Make(String)

module T = struct
  type t =
    { last_used_vars : String.Set.t
    ;  remove_never_used_vars : String.Set.t
    }
    [@@deriving sexp, equal, compare]
  
  let empty = { last_used_vars = String.Set.empty ; remove_never_used_vars = String.Set.empty }
end

include T