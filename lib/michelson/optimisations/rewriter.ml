module Tezos_micheline = Tezos_micheline
module Michelson = Michelson
module Smartpy_lltz_interopt = Smartpy_lltz_interopt
module Oasis_core = Oasis_core

let optimise_micheline (node : (unit, Michelson.Ast.Prim.t) Tezos_micheline.Micheline.node) : (unit, Michelson.Ast.Prim.t) Tezos_micheline.Micheline.node =
  let oasis_micheline = Smartpy_lltz_interopt.micheline_to_oasis_micheline node in
  let code = Oasis_core.Michelson.Of_micheline.instruction oasis_micheline in

  (* Optimise the code here *)
  let optimised_code = Oasis_core.Michelson_rewriter.run (Oasis_core.Michelson_rewriter.simplify Michelson_base.Protocol.Paris) code in

  let oasis_micheline_list = Oasis_core.Michelson.To_micheline.instruction Michelson_base.Protocol.Paris optimised_code in
  let oasis_micheline = Oasis_core.Micheline.Sequence (oasis_micheline_list) in
  Smartpy_lltz_interopt.oasis_micheline_to_micheline oasis_micheline