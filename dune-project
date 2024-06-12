(lang dune 2.0)

(name lltz)

(generate_opam_files true)

(source
 (github trilitech/lltz))

(license MIT)

(authors "TriliTech <contact@trili.tech>")

(maintainers "TriliTech <contact@trili.tech>")

(package
 (name lltz)
 (synopsis "A common compiler IR for targetting Michelson, the stack-based language of the Tezos blockchain.")
 (depends
  (dune
   (>= "2.0"))
  (ocaml
   (>= 4.08.0))
  (octez-libs.micheline
   (>= 1.73.0))
  (cmdliner
   (>= 1.1.0))))
