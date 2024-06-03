(lang dune 2.0)

(name lltz_ir)

(generate_opam_files true)

(source
 (github trilitech/lltz-draft-implementation))

(license Unlicense)

(authors "Sam Finch  <sam.finch@trili.tech>")

(maintainers "Sam Finch  <sam.finch@trili.tech>")

(package
 (name lltz_ir)
 (synopsis "Darft implementation of lltz ir")
 (description
  "TODO")
 (depends
  (dune
   (>= "2.0"))
  (ocaml
   (>= 4.08.0))
  (octez-libs.micheline
   (>= 1.73.0))
  (cmdliner
   (>= 1.1.0))))
