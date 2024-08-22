open Core 
open Tezos_micheline

module Prim = struct
  module Keyword = struct
    type t =
      | Parameter
      | Storage
      | Code
      | View
    [@@deriving equal, compare, sexp]

    let to_string = function
      | Parameter -> "parameter"
      | Storage -> "storage"
      | Code -> "code"
      | View -> "view"
    ;;

    let of_string = function
      | "parameter" -> Parameter
      | "storage" -> Storage
      | "code" -> Code
      | "view" -> View
      | received ->
        raise_s
          [%message
            "Michelson.Prim.Keyword.of_string: one of 'parameter', 'storage', 'code', \
             'view' is expected"
              (received : string)]
    ;;
  end

  module Instruction = struct
    type t =
      | Pack
      | Unpack
      | Blake2b
      | Sha256
      | Sha512
      | Abs
      | Add
      | Amount
      | And
      | Balance
      | Car
      | Cdr
      | Chain_id
      | Check_signature
      | Compare
      | Concat
      | Cons
      | Create_account
      | Create_contract
      | Implicit_account
      | Dip
      | Drop
      | Dup
      | View
      | Ediv
      | Empty_big_map
      | Empty_map
      | Empty_set
      | Eq
      | Exec
      | Apply
      | Failwith
      | Ge
      | Get
      | Get_and_update
      | Gt
      | Hash_key
      | If
      | If_cons
      | If_left
      | If_none
      | Int
      | Lambda
      | Lambda_rec
      | Le
      | Left
      | Level
      | Loop
      | Lsl
      | Lsr
      | Lt
      | Map
      | Mem
      | Mul
      | Neg
      | Neq
      | Nil
      | None
      | Not
      | Now
      | Min_block_time
      | Or
      | Pair
      | Unpair
      | Push
      | Right
      | Size
      | Some
      | Source
      | Sender
      | Self
      | Self_address
      | Slice
      | Steps_to_quota
      | Sub
      | Sub_mutez
      | Swap
      | Transfer_tokens
      | Set_delegate
      | Unit
      | Update
      | Xor
      | Iter
      | Loop_left
      | Address
      | Contract
      | Is_nat
      | Cast
      | Rename
      | Sapling_empty_state
      | Sapling_verify_update
      | Dig
      | Dug
      | Never
      | Voting_power
      | Total_voting_power
      | Keccak
      | Sha3
      | Pairing_check
      | Ticket
      | Ticket_deprecated
      | Read_ticket
      | Split_ticket
      | Join_tickets
      | Open_chest
      | Emit
      | Bytes
      | Nat
    [@@deriving equal, compare, sexp]

    let to_string : t -> string = function
      | Pack -> "PACK"
      | Unpack -> "UNPACK"
      | Blake2b -> "BLAKE2B"
      | Sha256 -> "SHA256"
      | Sha512 -> "SHA512"
      | Abs -> "ABS"
      | Add -> "ADD"
      | Amount -> "AMOUNT"
      | And -> "AND"
      | Balance -> "BALANCE"
      | Car -> "CAR"
      | Cdr -> "CDR"
      | Chain_id -> "CHAIN_ID"
      | Check_signature -> "CHECK_SIGNATURE"
      | Compare -> "COMPARE"
      | Concat -> "CONCAT"
      | Cons -> "CONS"
      | Create_account -> "CREATE_ACCOUNT"
      | Create_contract -> "CREATE_CONTRACT"
      | Implicit_account -> "IMPLICIT_ACCOUNT"
      | Dip -> "DIP"
      | Drop -> "DROP"
      | Dup -> "DUP"
      | View -> "VIEW"
      | Ediv -> "EDIV"
      | Empty_big_map -> "EMPTY_BIG_MAP"
      | Empty_map -> "EMPTY_MAP"
      | Empty_set -> "EMPTY_SET"
      | Eq -> "EQ"
      | Exec -> "EXEC"
      | Apply -> "APPLY"
      | Failwith -> "FAILWITH"
      | Ge -> "GE"
      | Get -> "GET"
      | Get_and_update -> "GET_AND_UPDATE"
      | Gt -> "GT"
      | Hash_key -> "HASH_KEY"
      | If -> "IF"
      | If_cons -> "IF_CONS"
      | If_left -> "IF_LEFT"
      | If_none -> "IF_NONE"
      | Int -> "INT"
      | Lambda -> "LAMBDA"
      | Lambda_rec -> "LAMBDA_REC"
      | Le -> "LE"
      | Left -> "LEFT"
      | Level -> "LEVEL"
      | Loop -> "LOOP"
      | Lsl -> "LSL"
      | Lsr -> "LSR"
      | Lt -> "LT"
      | Map -> "MAP"
      | Mem -> "MEM"
      | Mul -> "MUL"
      | Neg -> "NEG"
      | Neq -> "NEQ"
      | Nil -> "NIL"
      | None -> "NONE"
      | Not -> "NOT"
      | Now -> "NOW"
      | Min_block_time -> "MIN_BLOCK_TIME"
      | Or -> "OR"
      | Pair -> "PAIR"
      | Unpair -> "UNPAIR"
      | Push -> "PUSH"
      | Right -> "RIGHT"
      | Size -> "SIZE"
      | Some -> "SOME"
      | Source -> "SOURCE"
      | Sender -> "SENDER"
      | Self -> "SELF"
      | Self_address -> "SELF_ADDRESS"
      | Slice -> "SLICE"
      | Steps_to_quota -> "STEPS_TO_QUOTA"
      | Sub -> "SUB"
      | Sub_mutez -> "SUB_MUTEZ"
      | Swap -> "SWAP"
      | Transfer_tokens -> "TRANSFER_TOKENS"
      | Set_delegate -> "SET_DELEGATE"
      | Unit -> "UNIT"
      | Update -> "UPDATE"
      | Xor -> "XOR"
      | Iter -> "ITER"
      | Loop_left -> "LOOP_LEFT"
      | Address -> "ADDRESS"
      | Contract -> "CONTRACT"
      | Is_nat -> "ISNAT"
      | Cast -> "CAST"
      | Rename -> "RENAME"
      | Sapling_empty_state -> "SAPLING_EMPTY_STATE"
      | Sapling_verify_update -> "SAPLING_VERIFY_UPDATE"
      | Dig -> "DIG"
      | Dug -> "DUG"
      | Never -> "NEVER"
      | Voting_power -> "VOTING_POWER"
      | Total_voting_power -> "TOTAL_VOTING_POWER"
      | Keccak -> "KECCAK"
      | Sha3 -> "SHA3"
      | Pairing_check -> "PAIRING_CHECK"
      | Ticket -> "TICKET"
      | Ticket_deprecated -> "TICKET_DEPRECATED"
      | Read_ticket -> "READ_TICKET"
      | Split_ticket -> "SPLIT_TICKET"
      | Join_tickets -> "JOIN_TICKETS"
      | Open_chest -> "OPEN_CHEST"
      | Emit -> "EMIT"
      | Bytes -> "BYTES"
      | Nat -> "NAT"
    ;;

    let of_string : string -> t = function
      | "PACK" -> Pack
      | "UNPACK" -> Unpack
      | "BLAKE2B" -> Blake2b
      | "SHA256" -> Sha256
      | "SHA512" -> Sha512
      | "ABS" -> Abs
      | "ADD" -> Add
      | "AMOUNT" -> Amount
      | "AND" -> And
      | "BALANCE" -> Balance
      | "CAR" -> Car
      | "CDR" -> Cdr
      | "CHAIN_ID" -> Chain_id
      | "CHECK_SIGNATURE" -> Check_signature
      | "COMPARE" -> Compare
      | "CONCAT" -> Concat
      | "CONS" -> Cons
      | "CREATE_ACCOUNT" -> Create_account
      | "CREATE_CONTRACT" -> Create_contract
      | "IMPLICIT_ACCOUNT" -> Implicit_account
      | "DIP" -> Dip
      | "DROP" -> Drop
      | "DUP" -> Dup
      | "EDIV" -> Ediv
      | "EMPTY_BIG_MAP" -> Empty_big_map
      | "EMPTY_MAP" -> Empty_map
      | "EMPTY_SET" -> Empty_set
      | "EQ" -> Eq
      | "EXEC" -> Exec
      | "APPLY" -> Apply
      | "FAILWITH" -> Failwith
      | "GE" -> Ge
      | "GET" -> Get
      | "GET_AND_UPDATE" -> Get_and_update
      | "GT" -> Gt
      | "HASH_KEY" -> Hash_key
      | "IF" -> If
      | "IF_CONS" -> If_cons
      | "IF_LEFT" -> If_left
      | "IF_NONE" -> If_none
      | "INT" -> Int
      | "LAMBDA" -> Lambda
      | "LAMBDA_REC" -> Lambda_rec
      | "LE" -> Le
      | "LEFT" -> Left
      | "LEVEL" -> Level
      | "LOOP" -> Loop
      | "LSL" -> Lsl
      | "LSR" -> Lsr
      | "LT" -> Lt
      | "MAP" -> Map
      | "MEM" -> Mem
      | "MUL" -> Mul
      | "NEG" -> Neg
      | "NEQ" -> Neq
      | "NIL" -> Nil
      | "NONE" -> None
      | "NOT" -> Not
      | "NOW" -> Now
      | "MIN_BLOCK_TIME" -> Min_block_time
      | "OR" -> Or
      | "PAIR" -> Pair
      | "UNPAIR" -> Unpair
      | "PAIRING_CHECK" -> Pairing_check
      | "PUSH" -> Push
      | "RIGHT" -> Right
      | "SHA3" -> Sha3
      | "SIZE" -> Size
      | "SOME" -> Some
      | "SOURCE" -> Source
      | "SENDER" -> Sender
      | "SELF" -> Self
      | "SELF_ADDRESS" -> Self_address
      | "SLICE" -> Slice
      | "STEPS_TO_QUOTA" -> Steps_to_quota
      | "SUB" -> Sub
      | "SUB_MUTEZ" -> Sub_mutez
      | "SWAP" -> Swap
      | "TRANSFER_TOKENS" -> Transfer_tokens
      | "SET_DELEGATE" -> Set_delegate
      | "UNIT" -> Unit
      | "UPDATE" -> Update
      | "XOR" -> Xor
      | "ITER" -> Iter
      | "LOOP_LEFT" -> Loop_left
      | "ADDRESS" -> Address
      | "CONTRACT" -> Contract
      | "ISNAT" -> Is_nat
      | "CAST" -> Cast
      | "RENAME" -> Rename
      | "SAPLING_EMPTY_STATE" -> Sapling_empty_state
      | "SAPLING_VERIFY_UPDATE" -> Sapling_verify_update
      | "DIG" -> Dig
      | "DUG" -> Dug
      | "NEVER" -> Never
      | "VOTING_POWER" -> Voting_power
      | "TOTAL_VOTING_POWER" -> Total_voting_power
      | "KECCAK" -> Keccak
      | "TICKET" -> Ticket
      | "TICKET_DEPRECATED" -> Ticket_deprecated
      | "READ_TICKET" -> Read_ticket
      | "SPLIT_TICKET" -> Split_ticket
      | "JOIN_TICKETS" -> Join_tickets
      | "OPEN_CHEST" -> Open_chest
      | "EMIT" -> Emit
      | "VIEW" -> View
      | "BYTES" -> Bytes
      | "NAT" -> Nat
      | received ->
        raise_s
          [%message
            "Michelson.Prim.Instruction.of_string: unexpected input" (received : string)]
    ;;
  end

  module Constant = struct
    type t =
      | False
      | Elt
      | Left
      | None
      | Pair
      | Right
      | Some
      | True
      | Unit
      | Lambda_rec (* Serialised representation of recursive lambda functions *)
    [@@deriving equal, compare, sexp]

    let to_string = function
      | False -> "False"
      | Elt -> "Elt"
      | Left -> "Left"
      | None -> "None"
      | Pair -> "Pair"
      | Right -> "Right"
      | Some -> "Some"
      | True -> "True"
      | Unit -> "Unit"
      | Lambda_rec -> "Lambda_rec"
    ;;

    let of_string = function
      | "False" -> False
      | "Elt" -> Elt
      | "Left" -> Left
      | "None" -> None
      | "Pair" -> Pair
      | "Right" -> Right
      | "Some" -> Some
      | "True" -> True
      | "Unit" -> Unit
      | "Lambda_rec" -> Lambda_rec
      | received ->
        raise_s
          [%message
            "Michelson.Prim.Constant.of_string: unexpected input" (received : string)]
    ;;
  end

  module Type = struct
    type t =
      | Bool
      | Contract
      | Int
      | Key
      | Key_hash
      | Lambda
      | List
      | Map
      | Big_map
      | Nat
      | Option
      | Or
      | Pair
      | Set
      | Signature
      | String
      | Bytes
      | Mutez
      | Timestamp
      | Unit
      | Operation
      | Address
      | Tx_rollup_l2_address
      | Sapling_transaction
      | Sapling_transaction_deprecated
      | Sapling_state
      | Chain_id
      | Never
      | Bls12_381_g1
      | Bls12_381_g2
      | Bls12_381_fr
      | Ticket
      | Chest_key
      | Chest
    [@@deriving equal, compare, sexp]

    let to_string = function
      | Bool -> "bool"
      | Contract -> "contract"
      | Int -> "int"
      | Key -> "key"
      | Key_hash -> "key_hash"
      | Lambda -> "lambda"
      | List -> "list"
      | Map -> "map"
      | Big_map -> "big_map"
      | Nat -> "nat"
      | Option -> "option"
      | Or -> "or"
      | Pair -> "pair"
      | Set -> "set"
      | Signature -> "signature"
      | String -> "string"
      | Bytes -> "bytes"
      | Mutez -> "mutez"
      | Timestamp -> "timestamp"
      | Unit -> "unit"
      | Operation -> "operation"
      | Address -> "address"
      | Tx_rollup_l2_address -> "tx_rollup_l2_address"
      | Sapling_transaction -> "sapling_transaction"
      | Sapling_transaction_deprecated -> "sapling_transaction_deprecated"
      | Sapling_state -> "sapling_state"
      | Chain_id -> "chain_id"
      | Never -> "never"
      | Bls12_381_g1 -> "bls12_381_g1"
      | Bls12_381_g2 -> "bls12_381_g2"
      | Bls12_381_fr -> "bls12_381_fr"
      | Ticket -> "ticket"
      | Chest_key -> "chest_key"
      | Chest -> "chest"
    ;;

    let of_string = function
      | "bool" -> Bool
      | "contract" -> Contract
      | "int" -> Int
      | "key" -> Key
      | "key_hash" -> Key_hash
      | "lambda" -> Lambda
      | "list" -> List
      | "map" -> Map
      | "big_map" -> Big_map
      | "nat" -> Nat
      | "option" -> Option
      | "or" -> Or
      | "pair" -> Pair
      | "set" -> Set
      | "signature" -> Signature
      | "string" -> String
      | "bytes" -> Bytes
      | "mutez" -> Mutez
      | "timestamp" -> Timestamp
      | "unit" -> Unit
      | "operation" -> Operation
      | "address" -> Address
      | "tx_rollup_l2_address" -> Tx_rollup_l2_address
      | "sapling_transaction" -> Sapling_transaction
      | "sapling_transaction_deprecated" -> Sapling_transaction_deprecated
      | "sapling_state" -> Sapling_state
      | "chain_id" -> Chain_id
      | "never" -> Never
      | "bls12_381_g1" -> Bls12_381_g1
      | "bls12_381_g2" -> Bls12_381_g2
      | "bls12_381_fr" -> Bls12_381_fr
      | "ticket" -> Ticket
      | "chest_key" -> Chest_key
      | "chest" -> Chest
      | received ->
        raise_s
          [%message "Michelson.Prim.Type.of_string: unexpected input" (received : string)]
    ;;
  end

  module Global_storage = struct
    type t = Constant [@@deriving equal, compare, sexp]

    let to_string Constant = "constant"

    let of_string = function
      | "constant" -> Constant
      | received ->
        raise_s
          [%message
            "Michelson.Prim.Global_storage.of_string: 'constant' is expected"
              (received : string)]
    ;;
  end

  type t =
    | K of Keyword.t
    | I of Instruction.t
    | T of Type.t
    | C of Constant.t
    | G of Global_storage.t
  [@@deriving equal, compare, sexp]

  let to_string = function 
    | K keyword -> Keyword.to_string keyword
    | I instr -> Instruction.to_string instr
    | T type_ -> Type.to_string type_
    | C const -> Constant.to_string const
    | G global_storage -> Global_storage.to_string global_storage
  ;;

  let of_string received =
    let rec loop fs =
      match fs with
      | f :: fs ->
        (try f received with
        | _ -> loop fs)
      | [] ->
        raise_s
          [%message "Michelson.Prim.of_string: unexpected input" (received : string)]
    in
    loop
      [ (fun str -> K (Keyword.of_string str))
      ; (fun str -> C (Constant.of_string str))
      ; (fun str -> I (Instruction.of_string str))
      ; (fun str -> T (Type.of_string str))
      ; (fun str -> G (Global_storage.of_string str))
      ]
  ;;

  (*let to_michelson_v1_protocol_prim t =
    let open Tezos_protocol_alpha.Protocol in
    match Michelson_v1_primitives.prim_of_string (to_string t) with
    | Ok prim -> prim
    | Error _err ->
      raise_s
        [%message
          "Michelson.Ast.Prim.to_michelson_v1_protocol_prim: failed to convert." (t : t)]
  ;;*)
end

module Annot = struct
  (** Annotations attached to Michelson expressions *)
  type t = string list [@@deriving equal, compare, sexp]
end

type t = (unit, Prim.t) Micheline.node

let iter (t : t) ~f =
  let rec loop : t -> unit = function
    | Int ((), _) -> ()
    | String ((), _) -> ()
    | Bytes ((), _) -> ()
    | Prim ((), prim, ts, _) ->
      f prim;
      List.iter ts ~f:loop
    | Seq ((), ts) -> List.iter ts ~f:loop
  in
  loop t
;;

let analyze_instructions t =
  let tbl : (string, int) Hashtbl.t = Hashtbl.create (module String) in
  iter t ~f:(fun prim ->
      match prim with
      | I instr -> Hashtbl.incr tbl (Prim.Instruction.to_string instr)
      | _ -> ());
  tbl
;;

let pp ppf t =
  let open Micheline_printer in
  t |> Micheline.strip_locations |> printable Prim.to_string |> print_expr ppf
;;

(* "Smart constructors" *)
let seq ts : t = Seq ((), ts)
let prim ?(arguments = []) ?(annot = []) prim : t = Prim ((), prim, arguments, annot)
let int n : t = Int ((), Z.of_int n)
let string str : t = String ((), str)
let bytes str : t = Bytes ((), str)
let true_ : t = prim (C True)
let false_ : t = prim (C False)

module Type = struct
  type nonrec t = t

  let address = prim (T Address)
  let big_map key value = prim ~arguments:[ key; value ] (T Big_map)
  let bls12_381_fr = prim (T Bls12_381_fr)
  let bls12_381_g1 = prim (T Bls12_381_g1)
  let bls12_381_g2 = prim (T Bls12_381_g2)
  let bool = prim (T Bool)
  let bytes = prim (T Bytes)
  let chain_id = prim (T Chain_id)
  let contract type_ = prim ~arguments:[ type_ ] (T Contract)
  let int : t = prim (T Int)
  let key = prim (T Key)
  let key_hash = prim (T Key_hash)
  let lambda arg ret = prim ~arguments:[ arg; ret ] (T Lambda)
  let list type_ = prim ~arguments:[ type_ ] (T List)
  let map key value = prim ~arguments:[ key; value ] (T Map)
  let mutez = prim (T Mutez)
  let nat = prim (T Nat)
  let never = prim (T Never)
  let operation = prim (T Operation)
  let option ty = prim ~arguments:[ ty ] (T Option)
  let or_ t1 t2 = prim ~arguments:[ t1; t2 ] (T Or)

  let pair ts =
    assert (List.length ts >= 2);
    prim ~arguments:ts (T Pair)
  ;;

  let sampling_state n = prim ~arguments:[ n ] (T Sapling_state)
  let sapling_transaction n = prim ~arguments:[ n ] (T Sapling_transaction)
  let set cty = prim ~arguments:[ cty ] (T Set)
  let signature = prim (T Signature)
  let string = prim (T String)
  let ticket cty = prim ~arguments:[ cty ] (T Ticket)
  let timestamp = prim (T Timestamp)
  let unit = prim (T Unit)

  let chest = prim (T Chest)

  let chest_key = prim (T Chest_key)
end

module Instruction = struct
  type nonrec t = t

  let abs = prim (I Abs)
  let add = prim (I Add)
  let address = prim (I Address)
  let amount = prim (I Amount)
  let and_ = prim (I And)
  let apply = prim (I Apply)
  let balance = prim (I Balance)
  let blake2b = prim (I Blake2b)
  let car = prim (I Car)
  let cdr = prim (I Cdr)
  let chain_id = prim (I Chain_id)
  let check_signature = prim (I Check_signature)
  let compare = prim (I Compare)
  let concat = prim (I Concat)
  let cons = prim (I Cons)
  let contract ty = prim ~arguments:[ ty ] (I Contract)
  let create_contract ty1 ty2 instr1 = prim ~arguments:[ ty1; ty2; instr1 ] (I Create_contract) (*Any reason why this was just Contract before?*)
  let dig_n n = prim ~arguments:[ int n ] (I Dig)
  let dip instr = prim ~arguments:[ seq instr ] (I Dip)
  let dip_n n instr = prim ~arguments:[ int n; seq instr ] (I Dip)
  let drop = prim (I Drop)
  let drop_n n = prim ~arguments:[ int n ] (I Drop)
  let dug_n n = prim ~arguments:[ int n ] (I Dug)
  let dup = prim (I Dup)
  let dup_n n = prim ~arguments:[ int n ] (I Dup)
  let ediv = prim (I Ediv)
  let empty_big_map kty vty = prim ~arguments:[ kty; vty ] (I Empty_big_map)
  let empty_map kty vty = prim ~arguments:[ kty; vty ] (I Empty_map)
  let empty_set kty vty = prim ~arguments:[ kty; vty ] (I Empty_set)
  let eq = prim (I Eq)
  let exec = prim (I Exec)
  let failwith = prim (I Failwith)
  let ge = prim (I Ge)
  let get = prim (I Get)
  let get_n n = prim ~arguments:[ int n ] (I Get)
  let get_and_update = prim (I Get_and_update)
  let gt = prim (I Gt)
  let hash_key = prim (I Hash_key)
  let if_ ~then_ ~else_ = prim ~arguments:[ seq then_; seq else_ ] (I If)
  let if_cons ~then_ ~else_ = prim ~arguments:[ seq then_; seq else_ ] (I If_cons)
  let if_left ~then_ ~else_ = prim ~arguments:[ seq then_; seq else_ ] (I If_left)
  let if_none ~then_ ~else_ = prim ~arguments:[ seq then_; seq else_ ] (I If_none)
  let implicit_account = prim (I Implicit_account)
  let is_nat = prim (I Is_nat)
  let iter instrs = prim ~arguments:[ seq instrs ] (I Iter)
  let join_tickets = prim (I Join_tickets)
  let keccak = prim (I Keccak)
  let lambda ty1 ty2 instrs = prim ~arguments:[ ty1; ty2; seq instrs ] (I Lambda)
  let lambda_rec ty1 ty2 instrs = prim ~arguments:[ ty1; ty2; seq instrs ] (I Lambda_rec)
  let le = prim (I Le)
  let left ty2 = prim ~arguments:[ ty2 ] (I Left)
  let level = prim (I Level)
  let loop instr = prim ~arguments:[ seq instr ] (I Loop)
  let loop_left instr = prim ~arguments:[ seq instr ] (I Loop_left)
  let lsl_ = prim (I Lsl)
  let lsr_ = prim (I Lsr)
  let lt = prim (I Lt)
  let map instr = prim ~arguments:[ seq instr ] (I Map)
  let mem = prim (I Mem)
  let neg = prim (I Neg)
  let mul = prim (I Mul)
  let neq = prim (I Neq)
  let never = prim (I Never)
  let nil ty = prim ~arguments:[ ty ] (I Nil)
  let none ty = prim ~arguments:[ ty ] (I None)
  let not = prim (I Not)
  let now = prim (I Now)
  let or_ = prim (I Or)
  let pack = prim (I Pack)
  let pair = prim (I Pair)
  let pair_n n = prim ~arguments:[ int n ] (I Pair)
  let pairing_check = prim (I Pairing_check)
  let push ty x = prim ~arguments:[ ty; x ] (I Push)
  let read_ticket = prim (I Read_ticket)
  let right ty1 = prim ~arguments:[ ty1 ] (I Right)
  let sapling_empty_state ms = prim ~arguments:[ ms ] (I Sapling_empty_state)
  let self = prim (I Self)
  let self_address = prim (I Self_address)
  let sender = prim (I Sender)
  let set_delegate = prim (I Set_delegate)
  let sha256 = prim (I Sha256)
  let sha3 = prim (I Sha3)
  let sha512 = prim (I Sha512)
  let size = prim (I Size)
  let slice = prim (I Slice)
  let some = prim (I Some)
  let source = prim (I Source)
  let split_ticket = prim (I Split_ticket)
  let sub = prim (I Sub)
  let swap = prim (I Swap)
  let ticket = prim (I Ticket)
  let total_voting_power = prim (I Total_voting_power)
  let transfer_tokens = prim (I Transfer_tokens)
  let unit = prim (I Unit)
  let unpack ty = prim ~arguments:[ ty ] (I Unpack)
  let unpair = prim (I Unpair)
  let unpair_n n = prim ~arguments:[ int n ] (I Unpair)
  let update = prim (I Update)
  let update_n n = prim ~arguments:[ int n ] (I Update)
  let voting_power = prim (I Voting_power)
  let xor = prim (I Xor)
  let int = prim (I Int)

  (*Sequence*)
  (*Empty sequence*)
end

module Contract = struct
  type nonrec t =
    { parameter : t
    ; storage : t
    ; code : t
    }

  let dummy code =
    { parameter = Type.unit
    ; storage = Type.unit
    ; code = seq Instruction.([ drop ] @ code @ [ drop; unit; nil Type.operation; pair ])
    }
  ;;

  let to_t { parameter; storage; code } =
    seq
      [ prim ~arguments:[ parameter ] (K Parameter)
      ; prim ~arguments:[ storage ] (K Storage)
      ; prim ~arguments:[ code ] (K Code)
      ]
  ;;
end