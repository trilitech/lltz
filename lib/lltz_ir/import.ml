include Core

module Traverse_builtins = struct
  include Ppxlib_traverse_builtins

  let map_zero = Fn.id
  let iter_zero = ignore
  let fold_zero _t acc = acc
  let fold_map_zero t acc = t, acc
  let map_with_context_zero _ctx t = t
end
