open Core
include String

let create =
  let next = ref 0 in
  fun ?(prefix = "_") () ->
    Int.incr next;
    prefix ^ Int.to_string !next
;;
