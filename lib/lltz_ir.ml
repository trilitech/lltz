let hello name = Printf.printf "Hello, %s!\n" name

open Recursion

type 'deco type_ = 'deco Cofree(Type).t
type 'deco expr = 'deco Cofree(Expr).t
