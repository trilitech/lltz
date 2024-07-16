type t = [ `Base58 of string ]

let sha256 s = Digestif.SHA256.(to_raw_string (digest_string s))

let z58 = Z.of_int 58

let encode = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

let () = assert (String.length encode = 58)

let decode =
  let out = Bytes.make 256 '\255' in
  String.iteri
    (fun i char -> Bytes.set out (int_of_char char) (char_of_int i))
    encode;
  Bytes.to_string out

let of_char x =
  let pos = String.get decode (int_of_char x) in
  if pos = '\255' then failwith "Invalid data";
  int_of_char pos

let to_char x = encode.[x]

let count_leading_char s c =
  let len = String.length s in
  let rec loop i =
    if i = len then len else if String.get s i <> c then i else loop (i + 1)
  in
  loop 0

let count_trailing_char s c =
  let len = String.length s in
  let rec loop i =
    if i < 0
    then len
    else if String.get s i <> c
    then len - i - 1
    else loop (i - 1)
  in
  loop (len - 1)

let raw_encode s =
  let len = String.length s in
  let s = String.init len (fun i -> String.get s (len - i - 1)) in
  let zero = encode.[0] in
  let zeros = count_trailing_char s '\000' in
  let res_len = ((len * 8) + 4) / 5 in
  let res = Bytes.make res_len '\000' in
  let s = Z.of_bits s in
  let rec loop s =
    if s = Z.zero
    then 0
    else
      let s, r = Z.div_rem s z58 in
      let i = loop s in
      Bytes.set res i (to_char (Z.to_int r));
      i + 1
  in
  let i = loop s in
  let res = Bytes.sub_string res 0 i in
  String.make zeros zero ^ res

let raw_decode s =
  let zero = encode.[0] in
  let zeros = count_leading_char s zero in
  let len = String.length s in
  let rec loop res i =
    if i = len
    then res
    else
      let x = Z.of_int (of_char (String.get s i)) in
      let res = Z.(add x (mul res z58)) in
      loop res (i + 1)
  in
  let res = Z.to_bits @@ loop Z.zero zeros in
  let res_tzeros = count_trailing_char res '\000' in
  let len = String.length res - res_tzeros in
  String.make zeros '\000'
  ^ String.init len (fun i -> String.get res (len - i - 1))

let checksum s =
  let hash = sha256 (sha256 s) in
  let res = Bytes.make 4 '\000' in
  Bytes.blit_string hash 0 res 0 4;
  Bytes.unsafe_to_string res

(* Append a 4-bytes cryptographic checksum before encoding string s *)
let of_bytes s = `Base58 (raw_encode (s ^ checksum s))

let to_bytes (`Base58 s) =
  let s = raw_decode s in
  let len = String.length s in
  let msg = String.sub s 0 (len - 4) in
  let msg_hash = String.sub s (len - 4) 4 in
  if msg_hash <> checksum msg then None else Some msg

let to_bytes_exn s =
  match to_bytes s with
  | None -> invalid_arg "Base58.safe_decode_exn"
  | Some s -> s
