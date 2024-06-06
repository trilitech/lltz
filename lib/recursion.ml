(* Base functor for a recursive type *)
(* TODO mare ppx_deriving stuff *)
module type Base_functor = sig
  type 'a t

  val map : ('a -> 'b) -> 'a t -> 'b t
end

(* Recursion logic can be factored out here. *)
(* Bacause of the decoration there's it no added boilerplate for recursion schemes so the *)
(* recursive type is identical to what wo would make if used a single recursive definition *)
module Fixpoint (F : Base_functor) = struct
  type 'deco t = { deco : 'deco; inner : 'deco t F.t }

  let map f =
    let rec iter { deco; inner } =
      { deco = f deco; inner = F.map iter inner }
    in
    iter

  (* Just because I noticed it's a cofree comonad and I'm that knida guy *)
  let extract { deco; _ } = deco

  let rec duplicate { inner; _ } =
    { deco = inner; inner = F.map duplicate inner }

  let extend f =
    let rec iter { inner; _ } = { deco = f inner; inner = F.map iter inner } in
    iter

  let unwrap { inner; _ } = inner

  (* Recursion schemee type folds, we'll add a lot to this *)
  (* what functions we end up with willl depend on the implementation *)

  (* catamorphism with added decorator parameter *)
  let fold (alg : 'deco -> 'a F.t -> 'a) : 'deco t -> 'a =
    let rec iter { deco; inner } = alg deco (F.map iter inner) in
    iter

  let unfold (coalg : 'a -> 'deco * 'a F.t) : 'a -> 'deco t =
    let rec iter a =
      let deco, fa = coalg a in
      { deco; inner = F.map iter fa }
    in
    iter

  (* histomorphism? *)
  let redecorate (alg : 'deco_a -> 'deco_b F.t -> 'deco_b) :
      'deco_a t -> 'deco_b t =
    let rec iter { deco; inner } =
      let inner = F.map iter inner in
      { deco = alg deco (F.map extract inner); inner }
    in
    iter
end
