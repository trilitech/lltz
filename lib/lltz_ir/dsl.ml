(* dsl.ml *)
open Grace
module LE = Expr
module LT = Type
module LP = Primitive

module T = struct
  type var = Var of string
  type mut_var = Mut_var of string

  let dummy : Range.t = Range.initial (`String { content = ""; name = Some "" })

  (* Constants *)
  let unit = LE.{ desc = LE.Const(Unit); range = dummy }
  let bool b = LE.{ desc = LE.Const(Bool b); range = dummy }
  let nat n = LE.{ desc = LE.Const(Nat (Z.of_int n)); range = dummy }
  let int n = LE.{ desc = LE.Const(Int (Z.of_int n)); range = dummy }
  let mutez n = LE.{ desc = LE.Const(Mutez (Z.of_int n)); range = dummy }
  let string s = LE.{ desc = LE.Const(String s); range = dummy }
  let key s = LE.{ desc = LE.Const(Key s); range = dummy }
  let key_hash s = LE.{ desc = LE.Const(Key_hash s); range = dummy }
  let bytes s = LE.{ desc = LE.Const(Bytes s); range=dummy }
  let chain_id s = LE.{ desc = LE.Const(Chain_id s); range = dummy }

  let address_const s = LE.{ desc = LE.Const(Address s); range = dummy }

  let timestamp s = LE.{ desc = LE.Const(Timestamp s); range = dummy }
  let bls12_381_g1 s = LE.{ desc = LE.Const(Bls12_381_g1 s); range = dummy }
  let bls12_381_g2 s = LE.{ desc = LE.Const(Bls12_381_g2 s); range = dummy }
  let bls12_381_fr s = LE.{ desc = LE.Const(Bls12_381_fr s); range=dummy }
  let signature s = LE.{ desc = LE.Const(Signature s); range = dummy }

  (* Variables *)
  let var name = (LE.Var name)
  let mut_var name = LE.Mut_var name

  (* Types *)
  let unit_ty = LT.{ desc = Unit; range = dummy }
  let bool_ty = LT.{ desc = Bool; range = dummy  }
  let nat_ty = LT.{ desc = Nat ; range = dummy }
  let int_ty = LT.{ desc = Int ; range = dummy }
  let mutez_ty = LT.{ desc = Mutez; range = dummy  }
  let string_ty = LT.{ desc = String; range = dummy  }
  let bytes_ty = LT.{ desc = Bytes ; range = dummy }
  let chain_id_ty = LT.{ desc = Chain_id; range = dummy  }
  let timestamp_ty = LT.{ desc = Timestamp; range = dummy  }
  let address_ty = LT.{ desc = Address; range = dummy  }
  let key_ty = LT.{ desc = Keys; range = dummy  }
  let key_hash_ty = LT.{ desc = Key_hash; range = dummy  }
  let signature_ty = LT.{ desc = Signature ; range = dummy }

  (* Expressions *)
  let variable var = LE.{ desc = Variable var; range = dummy  }
  let let_in ~var ~rhs ~in_ = LE.{ desc = Let_in { let_var = var; rhs; in_ }; range = dummy  }
  let lambda ~var ~return_type ~body = LE.{ desc = Lambda { lam_var = var; return_type; body }; range = dummy  }
  let lambda_rec ~var ~mu ~return_type ~body = LE.{ desc = Lambda_rec { lam_var = var; mu_var = mu; return_type; body }; range = dummy  }
  let app ~abs ~arg = LE.{ desc = App { abs; arg } ; range = dummy }
  let let_mut_in ~var ~rhs ~in_ = LE.{ desc = Let_mut_in { let_var = var; rhs; in_ } ; range = dummy }
  let deref var = LE.{ desc = Deref var; range = dummy  }
  let assign ~var ~value = LE.{ desc = Assign (var, value); range = dummy  }
  let if_bool ~condition ~then_ ~else_ = LE.{ desc = If_bool { condition; if_true = then_; if_false = else_ }; range = dummy  }
  let if_none ~subject ~none ~some = LE.{ desc = If_none { subject; if_none = none; if_some = some }; range = dummy  }
  let if_cons ~subject ~empty ~nonempty = LE.{ desc = If_cons { subject; if_empty = empty; if_nonempty = nonempty }; range = dummy  }
  let if_left ~subject ~left ~right = LE.{ desc = If_left { subject; if_left = left; if_right = right }; range = dummy  }
  let while_ ~invariant ~body = LE.{ desc = While { invariant; body }; range = dummy  }
  let while_left ~invariant ~body = LE.{ desc = While_left { invariant; body }; range = dummy  }
  let for_ ~index ~init ~invariant ~variant ~body = LE.{ desc = For { index; init; invariant; variant; body }; range = dummy  }
  let for_each ~indices ~collection ~body = LE.{ desc = For_each { indices; collection; body }; range = dummy  }
  let map ~collection ~map = LE.{ desc = Map { collection; map } ; range = dummy }
  let fold_left ~collection ~init ~fold = LE.{ desc = Fold_left { collection; init; fold }; range = dummy  }
  let fold_right ~collection ~init ~fold = LE.{ desc = Fold_right { collection; init; fold } ; range = dummy }
  let let_tuple_in ~components ~rhs ~in_ = LE.{ desc = Let_tuple_in { components; rhs; in_ } ; range = dummy }
  let tuple row = LE.{ desc = Tuple row ; range = dummy }
  let proj ~tuple ~path = LE.{ desc = Proj (tuple,path ); range = dummy  }
  let update_tuple ~tuple ~component ~update = LE.{ desc = Update { tuple; component; update }; range = dummy  }
  let inj ~path ~expr = LE.{ desc = Inj (path,expr); range = dummy  }
  let match_ ~subject ~cases = LE.{ desc = Match (subject, cases ); range = dummy }
  let raw_michelson node = LE.{ desc = Raw_michelson node ; range = dummy }
  let create_contract ~storage ~parameter ~code ~delegate ~initial_balance ~initial_storage = 
    LE.{ desc = Create_contract { storage; parameter; code; delegate; initial_balance; initial_storage }; range = dummy  }

  (* Primitives *)
  (* Arity 0 *)
  let amount = LE.{ desc = Prim (LP.Amount, []) ; range = dummy }
  let balance = LE.{ desc = Prim (LP.Balance, []); range = dummy  }
  let chain_id_prim = LE.{ desc = Prim (LP.Chain_id, []); range = dummy  }
  let level = LE.{ desc = Prim (LP.Level, []) ; range = dummy }
  let now = LE.{ desc = Prim (LP.Now, []); range = dummy  }
  let self ?opt () = LE.{ desc = Prim (LP.Self opt, []); range = dummy  }
  let self_address = LE.{ desc = Prim (LP.Self_address, []); range = dummy }
  let sender = LE.{ desc = Prim (LP.Sender, []) ; range = dummy }
  let source = LE.{ desc = Prim (LP.Source, []); range = dummy  }
  let total_voting_power = LE.{ desc = Prim (LP.Total_voting_power, []) ; range = dummy }
  let empty_bigmap ~key ~value = LE.{ desc = Prim (LP.Empty_bigmap (key, value), []); range = dummy  }
  let empty_map ~key ~value = LE.{ desc = Prim (LP.Empty_map (key, value), []) ; range = dummy }
  let empty_set ~ty = LE.{ desc = Prim (LP.Empty_set ty, []); range = dummy  }
  let nil ~ty = LE.{ desc = Prim (LP.Nil ty, []); range = dummy  }
  let none ~ty = LE.{ desc = Prim (LP.None ty, []) ; range = dummy }
  let sapling_empty_state ~memo = LE.{ desc = Prim (LP.Sapling_empty_state { memo }, []); range = dummy  }
  let unit_prim = LE.{ desc = Prim (LP.Unit, []); range = dummy  }

  (* Arity 1/2 *)
  let car pair = LE.{ desc = Prim (LP.Car, [pair]); range = dummy }
  let cdr pair = LE.{ desc = Prim (LP.Cdr, [pair]); range = dummy }
  let left ~opt1 ~opt2 ~ty = LE.{ desc = Prim (LP.Left (opt1, opt2, ty), []); range = dummy }
  let right ~opt1 ~opt2 ~ty = LE.{ desc = Prim (LP.Right (opt1, opt2, ty), []); range = dummy }
  let some value = LE.{ desc = Prim (LP.Some, [value]); range = dummy }
  let eq lhs rhs = LE.{ desc = Prim (LP.Eq, [lhs; rhs]); range = dummy }
  let abs value = LE.{ desc = Prim (LP.Abs, [value]); range = dummy }
  let neg value = LE.{ desc = Prim (LP.Neg, [value]); range = dummy }
  let nat_prim value = LE.{ desc = Prim (LP.Nat, [value]); range = dummy }
  let int_prim value = LE.{ desc = Prim (LP.Int, [value]); range = dummy }
  let bytes_prim value = LE.{ desc = Prim (LP.Bytes, [value]); range = dummy }
  let is_nat value = LE.{ desc = Prim (LP.Is_nat, [value]); range = dummy }
  let neq lhs rhs = LE.{ desc = Prim (LP.Neq, [lhs; rhs]); range = dummy }
  let le lhs rhs = LE.{ desc = Prim (LP.Le, [lhs; rhs]); range = dummy }
  let lt lhs rhs = LE.{ desc = Prim (LP.Lt, [lhs; rhs]); range = dummy }
  let ge lhs rhs = LE.{ desc = Prim (LP.Ge, [lhs; rhs]); range = dummy }
  let gt lhs rhs = LE.{ desc = Prim (LP.Gt, [lhs; rhs]); range = dummy }
  let not value = LE.{ desc = Prim (LP.Not, [value]); range = dummy }
  let size container = LE.{ desc = Prim (LP.Size, [container]); range = dummy }
  let address contract = LE.{ desc = Prim (LP.Address, [contract]); range = dummy }
  let implicit_account key_hash = LE.{ desc = Prim (LP.Implicit_account, [key_hash]); range = dummy }
  let contract opt ~ty = LE.{ desc = Prim (LP.Contract (opt, ty), []); range = dummy }
  let pack value = LE.{ desc = Prim (LP.Pack, [value]); range = dummy }
  let unpack ~ty = LE.{ desc = Prim (LP.Unpack ty, []); range = dummy }
  let hash_key key = LE.{ desc = Prim (LP.Hash_key, [key]); range = dummy }
  let blake2b bytes = LE.{ desc = Prim (LP.Blake2b, [bytes]); range = dummy }
  let sha256 bytes = LE.{ desc = Prim (LP.Sha256, [bytes]); range = dummy }
  let sha512 bytes = LE.{ desc = Prim (LP.Sha512, [bytes]); range = dummy }
  let keccak bytes = LE.{ desc = Prim (LP.Keccak, [bytes]); range = dummy }
  let sha3 bytes = LE.{ desc = Prim (LP.Sha3, [bytes]); range = dummy }
  let set_delegate delegate = LE.{ desc = Prim (LP.Set_delegate, [delegate]); range = dummy }
  let read_ticket ticket = LE.{ desc = Prim (LP.Read_ticket, [ticket]); range = dummy }
  let join_tickets ticket1 ticket2 = LE.{ desc = Prim (LP.Join_tickets, [ticket1; ticket2]); range = dummy }
  let pairing_check pairings = LE.{ desc = Prim (LP.Pairing_check, [pairings]); range = dummy }
  let voting_power key_hash = LE.{ desc = Prim (LP.Voting_power, [key_hash]); range = dummy }
  let getn n = LE.{ desc = Prim (LP.Getn n, []); range = dummy }
  let cast ty = LE.{ desc = Prim (LP.Cast ty, []); range = dummy }
  let rename opt = LE.{ desc = Prim (LP.Rename opt, []); range = dummy }
  let emit opt ty = LE.{ desc = Prim (LP.Emit (opt, ty), []); range = dummy }
  let failwith value = LE.{ desc = Prim (LP.Failwith, [value]); range = dummy }
  let never value = LE.{ desc = Prim (LP.Never, [value]); range = dummy }
  let pair first second = LE.{ desc = Prim (LP.Pair (first, second), []); range = dummy }
  let add lhs rhs = LE.{ desc = Prim (LP.Add, [lhs; rhs]); range = dummy }
  let mul lhs rhs = LE.{ desc = Prim (LP.Mul, [lhs; rhs]); range = dummy }
  let sub lhs rhs = LE.{ desc = Prim (LP.Sub, [lhs; rhs]); range = dummy }
  let sub_mutez lhs rhs = LE.{ desc = Prim (LP.Sub_mutez, [lhs; rhs]); range = dummy }
  let lsr_ lhs rhs = LE.{ desc = Prim (LP.Lsr, [lhs; rhs]); range = dummy }
  let lsl_ lhs rhs = LE.{ desc = Prim (LP.Lsl, [lhs; rhs]); range = dummy }
  let xor lhs rhs = LE.{ desc = Prim (LP.Xor, [lhs; rhs]); range = dummy }
  let ediv lhs rhs = LE.{ desc = Prim (LP.Ediv, [lhs; rhs]); range = dummy }
  let and_ lhs rhs = LE.{ desc = Prim (LP.And, [lhs; rhs]); range = dummy }
  let or_ lhs rhs = LE.{ desc = Prim (LP.Or, [lhs; rhs]); range = dummy }
  let cons head tail = LE.{ desc = Prim (LP.Cons, [head; tail]); range = dummy }
  let compare lhs rhs = LE.{ desc = Prim (LP.Compare, [lhs; rhs]); range = dummy }
  let concat1 str1 str2 = LE.{ desc = Prim (LP.Concat1, [str1; str2]); range = dummy }
  let concat2 bytes1 bytes2 = LE.{ desc = Prim (LP.Concat2, [bytes1; bytes2]); range = dummy }
  let get key collection = LE.{ desc = Prim (LP.Get, [key; collection]); range = dummy }
  let mem key collection = LE.{ desc = Prim (LP.Mem, [key; collection]); range = dummy }
  let exec value lambda = LE.{ desc = Prim (LP.Exec, [value; lambda;]); range = dummy }
  let apply value lambda = LE.{ desc = Prim (LP.Apply, [value; lambda;]); range = dummy }
  let sapling_verify_update transaction state = LE.{ desc = Prim (LP.Sapling_verify_update, [transaction; state]); range = dummy }
  let ticket content amount = LE.{ desc = Prim (LP.Ticket, [content; amount]); range = dummy }
  let ticket_deprecated content amount = LE.{ desc = Prim (LP.Ticket_deprecated, [content; amount]); range = dummy }
  let split_ticket ticket amounts = LE.{ desc = Prim (LP.Split_ticket, [ticket; amounts]); range = dummy }
  let updaten n value pair = LE.{ desc = Prim (LP.Updaten n, [value; pair]); range = dummy }
  let view ~name ~return_type = LE.{ desc = Prim (LP.View (name, return_type), []); range = dummy }

  (* Arity 3 *)
  let slice offset length sequence = LE.{ desc = Prim (LP.Slice, [offset; length; sequence]); range = dummy }
  let update key value collection = LE.{ desc = Prim (LP.Update, [key; value; collection]); range = dummy }
  let get_and_update key value collection = LE.{ desc = Prim (LP.Get_and_update, [key; value; collection]); range = dummy }
  let transfer_tokens param amount contract = LE.{ desc = Prim (LP.Transfer_tokens, [param; amount; contract]); range = dummy }
  let check_signature key signature message = LE.{ desc = Prim (LP.Check_signature, [key; signature; message]); range = dummy }
  let open_chest chest_key chest time = LE.{ desc = Prim (LP.Open_chest, [chest_key; chest; time]); range = dummy }



  let convert_list (exprs: LE.t list) : LE.t Row.t =
    let converted_row_leaves = List.map (fun ty -> Row.Leaf (None, ty)) exprs in
    Row.Node converted_row_leaves
end
