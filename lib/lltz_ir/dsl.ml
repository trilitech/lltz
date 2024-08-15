(* dsl.ml 
   This file adds a domain specific language (DSL), consisting of functions for easier creation of expressions using LLTZ-IR. 
*)
open Grace
module LLTZ = struct
  module E = Expr
  module T = Type
  module P = Primitive
end

module DSL = struct
  type var = Var of string
  type mut_var = Mut_var of string

  let dummy : Range.t = Range.initial (`String { content = ""; name = Some "" })

  (* Creation with optional range *)
  let create ?(range = dummy) desc = LLTZ.E.{ desc; range }

  (* Constants *)
  let unit ?(range = dummy) () = create ~range (LLTZ.E.Const(Unit))
  let bool ?(range = dummy) b = create ~range (LLTZ.E.Const(Bool b))
  let nat ?(range = dummy) n = create ~range (LLTZ.E.Const(Nat (Z.of_int n)))
  let int ?(range = dummy) n = create ~range (LLTZ.E.Const(Int (Z.of_int n)))
  let mutez ?(range = dummy) n = create ~range (LLTZ.E.Const(Mutez (Z.of_int n)))
  let string ?(range = dummy) s = create ~range (LLTZ.E.Const(String s))
  let key ?(range = dummy) s = create ~range (LLTZ.E.Const(Key s))
  let key_hash ?(range = dummy) s = create ~range (LLTZ.E.Const(Key_hash s))
  let bytes ?(range = dummy) s = create ~range (LLTZ.E.Const(Bytes s))
  let chain_id ?(range = dummy) s = create ~range (LLTZ.E.Const(Chain_id s))

  let address_const ?(range = dummy) s = create ~range (LLTZ.E.Const(Address s))

  let timestamp ?(range = dummy) s = create ~range (LLTZ.E.Const(Timestamp s))
  let bls12_381_g1 ?(range = dummy) s = create ~range (LLTZ.E.Const(Bls12_381_g1 s))
  let bls12_381_g2 ?(range = dummy) s = create ~range (LLTZ.E.Const(Bls12_381_g2 s))
  let bls12_381_fr ?(range = dummy) s = create ~range (LLTZ.E.Const(Bls12_381_fr s))
  let signature ?(range = dummy) s = create ~range (LLTZ.E.Const(Signature s))

  (* Variables *)
  let var name = LLTZ.E.Var name
  let mut_var name = LLTZ.E.Mut_var name

  (* Types *)
  let unit_ty ?(range = dummy) () = LLTZ.T.{ desc = Unit; range }
  let bool_ty ?(range = dummy) () = LLTZ.T.{ desc = Bool; range }
  let nat_ty ?(range = dummy) () = LLTZ.T.{ desc = Nat; range }
  let int_ty ?(range = dummy) () = LLTZ.T.{ desc = Int; range }
  let mutez_ty ?(range = dummy) () = LLTZ.T.{ desc = Mutez; range }
  let string_ty ?(range = dummy) () = LLTZ.T.{ desc = String; range }
  let bytes_ty ?(range = dummy) () = LLTZ.T.{ desc = Bytes; range }
  let chain_id_ty ?(range = dummy) () = LLTZ.T.{ desc = Chain_id; range }
  let timestamp_ty ?(range = dummy) () = LLTZ.T.{ desc = Timestamp; range }
  let address_ty ?(range = dummy) () = LLTZ.T.{ desc = Address; range }
  let key_ty ?(range = dummy) () = LLTZ.T.{ desc = Keys; range }
  let key_hash_ty ?(range = dummy) () = LLTZ.T.{ desc = Key_hash; range }
  let signature_ty ?(range = dummy) () = LLTZ.T.{ desc = Signature; range }

  (* Expressions *)
  let variable ?(range = dummy) var = create ~range (LLTZ.E.Variable var)
  let let_in ?(range = dummy) ~var ~rhs ~in_ = create ~range (LLTZ.E.Let_in { let_var = var; rhs; in_ })
  let lambda ?(range = dummy) ~var ~return_type ~body = create ~range (LLTZ.E.Lambda { lam_var = var; return_type; body })
  let lambda_rec ?(range = dummy) ~var ~mu ~return_type ~body = create ~range (LLTZ.E.Lambda_rec { lam_var = var; mu_var = mu; return_type; body })
  let app ?(range = dummy) ~abs ~arg = create ~range (LLTZ.E.App { abs; arg })
  let let_mut_in ?(range = dummy) ~var ~rhs ~in_ = create ~range (LLTZ.E.Let_mut_in { let_var = var; rhs; in_ })
  let deref ?(range = dummy) var = create ~range (LLTZ.E.Deref var)
  let assign ?(range = dummy) ~var ~value = create ~range (LLTZ.E.Assign (var, value))
  let if_bool ?(range = dummy) ~condition ~then_ ~else_ = create ~range (LLTZ.E.If_bool { condition; if_true = then_; if_false = else_ })
  let if_none ?(range = dummy) ~subject ~none ~some = create ~range (LLTZ.E.If_none { subject; if_none = none; if_some = some })
  let if_cons ?(range = dummy) ~subject ~empty ~nonempty = create ~range (LLTZ.E.If_cons { subject; if_empty = empty; if_nonempty = nonempty })
  let if_left ?(range = dummy) ~subject ~left ~right = create ~range (LLTZ.E.If_left { subject; if_left = left; if_right = right })
  let while_ ?(range = dummy) ~invariant ~body = create ~range (LLTZ.E.While { invariant; body })
  let while_left ?(range = dummy) ~invariant ~body = create ~range (LLTZ.E.While_left { invariant; body })
  let for_ ?(range = dummy) ~index ~init ~invariant ~variant ~body = create ~range (LLTZ.E.For { index; init; invariant; variant; body })
  let for_each ?(range = dummy) ~indices ~collection ~body = create ~range (LLTZ.E.For_each { indices; collection; body })
  let map ?(range = dummy) ~collection ~map = create ~range (LLTZ.E.Map { collection; map })
  let fold_left ?(range = dummy) ~collection ~init ~fold = create ~range (LLTZ.E.Fold_left { collection; init; fold })
  let fold_right ?(range = dummy) ~collection ~init ~fold = create ~range (LLTZ.E.Fold_right { collection; init; fold })
  let let_tuple_in ?(range = dummy) ~components ~rhs ~in_ = create ~range (LLTZ.E.Let_tuple_in { components; rhs; in_ })
  let tuple ?(range = dummy) row = create ~range (LLTZ.E.Tuple row)
  let proj ?(range = dummy) ~tuple ~path = create ~range (LLTZ.E.Proj (tuple, path))
  let update_tuple ?(range = dummy) ~tuple ~component ~update = create ~range (LLTZ.E.Update { tuple; component; update })
  let inj ?(range = dummy) ~path ~expr = create ~range (LLTZ.E.Inj (path, expr))
  let match_ ?(range = dummy) ~subject ~cases = create ~range (LLTZ.E.Match (subject, cases))
  let raw_michelson ?(range = dummy) node = create ~range (LLTZ.E.Raw_michelson node)
  let create_contract ?(range = dummy) ~storage ~parameter ~code ~delegate ~initial_balance ~initial_storage =
    create ~range (LLTZ.E.Create_contract { storage; parameter; code; delegate; initial_balance; initial_storage })

  (* Primitives *)
  (* Arity 0 *)
  let amount ?(range = dummy) () = create ~range (LLTZ.E.Prim (LLTZ.P.Amount, []))
  let balance ?(range = dummy) () = create ~range (LLTZ.E.Prim (LLTZ.P.Balance, []))
  let chain_id_prim ?(range = dummy) () = create ~range (LLTZ.E.Prim (LLTZ.P.Chain_id, []))
  let level ?(range = dummy) () = create ~range (LLTZ.E.Prim (LLTZ.P.Level, []))
  let now ?(range = dummy) () = create ~range (LLTZ.E.Prim (LLTZ.P.Now, []))
  let self ?(range = dummy) ?opt () = create ~range (LLTZ.E.Prim (LLTZ.P.Self opt, []))
  let self_address ?(range = dummy) () = create ~range (LLTZ.E.Prim (LLTZ.P.Self_address, []))
  let sender ?(range = dummy) () = create ~range (LLTZ.E.Prim (LLTZ.P.Sender, []))
  let source ?(range = dummy) () = create ~range (LLTZ.E.Prim (LLTZ.P.Source, []))
  let total_voting_power ?(range = dummy) () = create ~range (LLTZ.E.Prim (LLTZ.P.Total_voting_power, []))
  let empty_bigmap ?(range = dummy) ~key ~value = create ~range (LLTZ.E.Prim (LLTZ.P.Empty_bigmap (key, value), []))
  let empty_map ?(range = dummy) ~key ~value = create ~range (LLTZ.E.Prim (LLTZ.P.Empty_map (key, value), []))
  let empty_set ?(range = dummy) ~ty = create ~range (LLTZ.E.Prim (LLTZ.P.Empty_set ty, []))
  let nil ?(range = dummy) ~ty = create ~range (LLTZ.E.Prim (LLTZ.P.Nil ty, []))
  let none ?(range = dummy) ~ty = create ~range (LLTZ.E.Prim (LLTZ.P.None ty, []))
  let sapling_empty_state ?(range = dummy) ~memo = create ~range (LLTZ.E.Prim (LLTZ.P.Sapling_empty_state { memo }, []))
  let unit_prim ?(range = dummy) () = create ~range (LLTZ.E.Prim (LLTZ.P.Unit, []))

  (* Arity 1/2 *)
  let car ?(range = dummy) pair = create ~range (LLTZ.E.Prim (LLTZ.P.Car, [pair]))
  let cdr ?(range = dummy) pair = create ~range (LLTZ.E.Prim (LLTZ.P.Cdr, [pair]))
  let left ?(range = dummy) ~opt1 ~opt2 ~ty = create ~range (LLTZ.E.Prim (LLTZ.P.Left (opt1, opt2, ty), []))
  let right ?(range = dummy) ~opt1 ~opt2 ~ty = create ~range (LLTZ.E.Prim (LLTZ.P.Right (opt1, opt2, ty), []))
  let some ?(range = dummy) value = create ~range (LLTZ.E.Prim (LLTZ.P.Some, [value]))
  let eq ?(range = dummy) lhs rhs = create ~range (LLTZ.E.Prim (LLTZ.P.Eq, [lhs; rhs]))
  let abs ?(range = dummy) value = create ~range (LLTZ.E.Prim (LLTZ.P.Abs, [value]))
  let neg ?(range = dummy) value = create ~range (LLTZ.E.Prim (LLTZ.P.Neg, [value]))
  let nat_prim ?(range = dummy) value = create ~range (LLTZ.E.Prim (LLTZ.P.Nat, [value]))
  let int_prim ?(range = dummy) value = create ~range (LLTZ.E.Prim (LLTZ.P.Int, [value]))
  let bytes_prim ?(range = dummy) value = create ~range (LLTZ.E.Prim (LLTZ.P.Bytes, [value]))
  let is_nat ?(range = dummy) value = create ~range (LLTZ.E.Prim (LLTZ.P.Is_nat, [value]))
  let neq ?(range = dummy) lhs rhs = create ~range (LLTZ.E.Prim (LLTZ.P.Neq, [lhs; rhs]))
  let le ?(range = dummy) lhs rhs = create ~range (LLTZ.E.Prim (LLTZ.P.Le, [lhs; rhs]))
  let lt ?(range = dummy) lhs rhs = create ~range (LLTZ.E.Prim (LLTZ.P.Lt, [lhs; rhs]))
  let ge ?(range = dummy) lhs rhs = create ~range (LLTZ.E.Prim (LLTZ.P.Ge, [lhs; rhs]))
  let gt ?(range = dummy) lhs rhs = create ~range (LLTZ.E.Prim (LLTZ.P.Gt, [lhs; rhs]))
  let not ?(range = dummy) value = create ~range (LLTZ.E.Prim (LLTZ.P.Not, [value]))
  let size ?(range = dummy) container = create ~range (LLTZ.E.Prim (LLTZ.P.Size, [container]))
  let address ?(range = dummy) contract = create ~range (LLTZ.E.Prim (LLTZ.P.Address, [contract]))
  let implicit_account ?(range = dummy) key_hash = create ~range (LLTZ.E.Prim (LLTZ.P.Implicit_account, [key_hash]))
  let contract ?(range = dummy) opt ~ty = create ~range (LLTZ.E.Prim (LLTZ.P.Contract (opt, ty), []))
  let pack ?(range = dummy) value = create ~range (LLTZ.E.Prim (LLTZ.P.Pack, [value]))
  let unpack ?(range = dummy) ~ty = create ~range (LLTZ.E.Prim (LLTZ.P.Unpack ty, []))
  let hash_key ?(range = dummy) key = create ~range (LLTZ.E.Prim (LLTZ.P.Hash_key, [key]))
  let blake2b ?(range = dummy) bytes = create ~range (LLTZ.E.Prim (LLTZ.P.Blake2b, [bytes]))
  let sha256 ?(range = dummy) bytes = create ~range (LLTZ.E.Prim (LLTZ.P.Sha256, [bytes]))
  let sha512 ?(range = dummy) bytes = create ~range (LLTZ.E.Prim (LLTZ.P.Sha512, [bytes]))
  let keccak ?(range = dummy) bytes = create ~range (LLTZ.E.Prim (LLTZ.P.Keccak, [bytes]))
  let sha3 ?(range = dummy) bytes = create ~range (LLTZ.E.Prim (LLTZ.P.Sha3, [bytes]))
  let set_delegate ?(range = dummy) delegate = create ~range (LLTZ.E.Prim (LLTZ.P.Set_delegate, [delegate]))
  let read_ticket ?(range = dummy) ticket = create ~range (LLTZ.E.Prim (LLTZ.P.Read_ticket, [ticket]))
  let join_tickets ?(range = dummy) ticket1 ticket2 = create ~range (LLTZ.E.Prim (LLTZ.P.Join_tickets, [ticket1; ticket2]))
  let pairing_check ?(range = dummy) pairings = create ~range (LLTZ.E.Prim (LLTZ.P.Pairing_check, [pairings]))
  let voting_power ?(range = dummy) key_hash = create ~range (LLTZ.E.Prim (LLTZ.P.Voting_power, [key_hash]))
  let getn ?(range = dummy) n = create ~range (LLTZ.E.Prim (LLTZ.P.Getn n, []))
  let cast ?(range = dummy) ty = create ~range (LLTZ.E.Prim (LLTZ.P.Cast ty, []))
  let rename ?(range = dummy) opt = create ~range (LLTZ.E.Prim (LLTZ.P.Rename opt, []))
  let emit ?(range = dummy) opt ty = create ~range (LLTZ.E.Prim (LLTZ.P.Emit (opt, ty), []))
  let failwith ?(range = dummy) value = create ~range (LLTZ.E.Prim (LLTZ.P.Failwith, [value]))
  let never ?(range = dummy) value = create ~range (LLTZ.E.Prim (LLTZ.P.Never, [value]))
  let pair ?(range = dummy) first second = create ~range (LLTZ.E.Prim (LLTZ.P.Pair (first, second), []))
  let add ?(range = dummy) lhs rhs = create ~range (LLTZ.E.Prim (LLTZ.P.Add, [lhs; rhs]))
  let mul ?(range = dummy) lhs rhs = create ~range (LLTZ.E.Prim (LLTZ.P.Mul, [lhs; rhs]))
  let sub ?(range = dummy) lhs rhs = create ~range (LLTZ.E.Prim (LLTZ.P.Sub, [lhs; rhs]))
  let sub_mutez ?(range = dummy) lhs rhs = create ~range (LLTZ.E.Prim (LLTZ.P.Sub_mutez, [lhs; rhs]))
  let lsr_ ?(range = dummy) lhs rhs = create ~range (LLTZ.E.Prim (LLTZ.P.Lsr, [lhs; rhs]))
  let lsl_ ?(range = dummy) lhs rhs = create ~range (LLTZ.E.Prim (LLTZ.P.Lsl, [lhs; rhs]))
  let xor ?(range = dummy) lhs rhs = create ~range (LLTZ.E.Prim (LLTZ.P.Xor, [lhs; rhs]))
  let ediv ?(range = dummy) lhs rhs = create ~range (LLTZ.E.Prim (LLTZ.P.Ediv, [lhs; rhs]))
  let and_ ?(range = dummy) lhs rhs = create ~range (LLTZ.E.Prim (LLTZ.P.And, [lhs; rhs]))
  let or_ ?(range = dummy) lhs rhs = create ~range (LLTZ.E.Prim (LLTZ.P.Or, [lhs; rhs]))
  let cons ?(range = dummy) head tail = create ~range (LLTZ.E.Prim (LLTZ.P.Cons, [head; tail]))
  let compare ?(range = dummy) lhs rhs = create ~range (LLTZ.E.Prim (LLTZ.P.Compare, [lhs; rhs]))
  let concat1 ?(range = dummy) str1 str2 = create ~range (LLTZ.E.Prim (LLTZ.P.Concat1, [str1; str2]))
  let concat2 ?(range = dummy) bytes1 bytes2 = create ~range (LLTZ.E.Prim (LLTZ.P.Concat2, [bytes1; bytes2]))
  let get ?(range = dummy) key collection = create ~range (LLTZ.E.Prim (LLTZ.P.Get, [key; collection]))
  let mem ?(range = dummy) key collection = create ~range (LLTZ.E.Prim (LLTZ.P.Mem, [key; collection]))
  let exec ?(range = dummy) value lambda = create ~range (LLTZ.E.Prim (LLTZ.P.Exec, [value; lambda]))
  let apply ?(range = dummy) value lambda = create ~range (LLTZ.E.Prim (LLTZ.P.Apply, [value; lambda]))
  let sapling_verify_update ?(range = dummy) transaction state = create ~range (LLTZ.E.Prim (LLTZ.P.Sapling_verify_update, [transaction; state]))
  let ticket ?(range = dummy) content amount = create ~range (LLTZ.E.Prim (LLTZ.P.Ticket, [content; amount]))
  let ticket_deprecated ?(range = dummy) content amount = create ~range (LLTZ.E.Prim (LLTZ.P.Ticket_deprecated, [content; amount]))
  let split_ticket ?(range = dummy) ticket amounts = create ~range (LLTZ.E.Prim (LLTZ.P.Split_ticket, [ticket; amounts]))
  let updaten ?(range = dummy) n value pair = create ~range (LLTZ.E.Prim (LLTZ.P.Updaten n, [value; pair]))
  let view ?(range = dummy) ~name ~return_type = create ~range (LLTZ.E.Prim (LLTZ.P.View (name, return_type), []))

  (* Arity 3 *)
  let slice ?(range = dummy) offset length sequence = create ~range (LLTZ.E.Prim (LLTZ.P.Slice, [offset; length; sequence]))
  let update ?(range = dummy) key value collection = create ~range (LLTZ.E.Prim (LLTZ.P.Update, [key; value; collection]))
  let get_and_update ?(range = dummy) key value collection = create ~range (LLTZ.E.Prim (LLTZ.P.Get_and_update, [key; value; collection]))
  let transfer_tokens ?(range = dummy) param amount contract = create ~range (LLTZ.E.Prim (LLTZ.P.Transfer_tokens, [param; amount; contract]))
  let check_signature ?(range = dummy) key signature message = create ~range (LLTZ.E.Prim (LLTZ.P.Check_signature, [key; signature; message]))
  let open_chest ?(range = dummy) chest_key chest time = create ~range (LLTZ.E.Prim (LLTZ.P.Open_chest, [chest_key; chest; time]))

  let convert_list (exprs: LLTZ.E.t list) : LLTZ.E.t Row.t =
    let converted_row_leaves = List.map (fun ty -> Row.Leaf (None, ty)) exprs in
    Row.Node converted_row_leaves
end
