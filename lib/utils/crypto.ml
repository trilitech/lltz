module Blake2b_Hash_32 = Digestif.Make_BLAKE2B (struct
  let digest_size = 32
end)

let sha512 s = Digestif.SHA512.(digest_string s |> to_raw_string)

let sha256 s = Digestif.SHA256.(digest_string s |> to_raw_string)

let blake2b s = Blake2b_Hash_32.(digest_string s |> to_raw_string)

let sha3 s = Digestif.SHA3_256.(digest_string s |> to_raw_string)

let keccak s = Digestif.KECCAK_256.(digest_string s |> to_raw_string)
