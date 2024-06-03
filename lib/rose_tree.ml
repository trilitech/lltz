type ('k, 'v) t = Leaf of 'v | Node of ('k * ('k, 'v) t) list
type 'k path = 'k list

let map f =
  let rec iter = function
    | Leaf a -> Leaf (f a)
    | Node rts -> Node (List.map (fun (k, rt) -> (k, iter rt)) rts)
  in
  iter
