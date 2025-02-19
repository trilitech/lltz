(lang dune 3.8)

(name lltz)

(generate_opam_files true)

(source
 (github trilitech/lltz))

(license MIT)

(authors "TriliTech <contact@trili.tech>")

(maintainers "TriliTech <contact@trili.tech>")

(package
 (name lltz)
 (synopsis
  "A common compiler IR for targetting Michelson, the stack-based language of the Tezos blockchain.")
 (depends
  (ocaml
   (= 4.14.2))
   ptime
  dune
  core
  ppx_jane
  ppx_yojson_conv
  (linol
   (= 0.5))
  (linol-lwt
   (= 0.5))
  digestif
  ppxlib
  grace))
