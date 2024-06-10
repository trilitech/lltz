let hello name = Printf.printf "Hello, %s!\n" name

module Common = Common
module Type = Type
module Expr = Expr
module Rose_tree = Rose_tree

type type_ = Common.location Type.t
type expr = Common.location Expr.t
