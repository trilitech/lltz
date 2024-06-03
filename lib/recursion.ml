module type Base_functor = sig
  type 'a t

  val map : ('a -> 'b) -> 'a t -> 'b t
end

module Cofree (F : Base_functor) = struct
  type 'deco t = { deco : 'deco; inner : 'deco t F.t }

  let map f =
    let rec iter { deco; inner } =
      { deco = f deco; inner = F.map iter inner }
    in
    iter

  (*  comonad interface because I called it a cofree  *)
  let extract { deco; _ } = deco

  let rec duplicate { inner; _ } =
    { deco = inner; inner = F.map duplicate inner }

  let extend f =
    let rec iter { inner; _ } = { deco = f inner; inner = F.map iter inner } in
    iter

  let fold alg =
    let rec iter { deco; inner } = alg deco (F.map iter inner) in
    iter

  let unfold coalg =
    let rec iter a =
      let deco, fa = coalg a in
      { deco; inner = F.map iter fa }
    in
    iter

  (* histomorphism *)
  let redecorate alg =
    let rec iter { deco; inner } =
      let inner = F.map iter inner in
      let deco = alg deco (F.map (fun { deco; inner = _ } -> deco) inner) in
      { deco; inner }
    in
    iter
end
