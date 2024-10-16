module Tezos_micheline = Tezos_micheline
module Michelson = Michelson
module Oasis_core = Oasis_core

let rec micheline_to_oasis_micheline (node : (unit, Michelson.Ast.Prim.t) Tezos_micheline.Micheline.node) : Oasis_core.Micheline.t =
  match node with
  | Int (_, z) ->
      Int (Z.to_string z)
  | String (_, s) ->
      String s
  | Bytes (_, b) ->
      Bytes (Bytes.to_string b)
  | Prim (_, prim, args, annots) ->
      Primitive {
        name = Michelson.Ast.Prim.to_string prim;
        annotations = annots;
        arguments = List.map (micheline_to_oasis_micheline) args;
      }
  | Seq (_, nodes) ->
      Sequence (List.map (micheline_to_oasis_micheline) nodes)

let rec oasis_micheline_to_micheline (t : Oasis_core.Micheline.t) : (unit, Michelson.Ast.Prim.t) Tezos_micheline.Micheline.node =
  match t with
  | Int s ->
      Int ((), Z.of_string s)
  | String s ->
      String ((), s)
  | Bytes s ->
      (* Adjust based on whether 's' is raw bytes or hex-encoded *)
      let bytes =
        try
          (* If 's' is hex-encoded *)
          Hex.to_bytes (`Hex s)
        with Invalid_argument _ ->
          (* If 's' is raw bytes *)
          Bytes.of_string s
      in
      Bytes ((), bytes)
  | Primitive {name; annotations; arguments} ->
      let prim = Michelson.Ast.Prim.of_string name in
      let args = List.map (oasis_micheline_to_micheline) arguments in
      Prim ((), prim, args, annotations)
  | Sequence nodes ->
      let compiled_nodes = List.map (oasis_micheline_to_micheline) nodes in
      Seq ((), compiled_nodes)

let optimise_micheline (node : (unit, Michelson.Ast.Prim.t) Tezos_micheline.Micheline.node) : (unit, Michelson.Ast.Prim.t) Tezos_micheline.Micheline.node =
  let oasis_micheline = micheline_to_oasis_micheline node in
  let code = Oasis_core.Michelson.Of_micheline.instruction oasis_micheline in

  (* Optimise the code here *)
  let optimised_code = Oasis_core.Michelson_rewriter.run (Oasis_core.Michelson_rewriter.simplify) code in

  let oasis_micheline_list = Oasis_core.Michelson.To_micheline.instruction optimised_code in
  let oasis_micheline = Oasis_core.Micheline.Sequence (oasis_micheline_list) in
  oasis_micheline_to_micheline oasis_micheline