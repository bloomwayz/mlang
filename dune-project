(lang dune 3.15)

(using menhir 3.0)

(name lang_m)

(generate_opam_files true)

(source
 (github bloomwayz/mlang))

(authors
  "Jay Lee"
  "Junyoung Park")

(maintainers
  "Jay Lee"
  "Junyoung Park")

(license LICENSE)

; (documentation https://url/to/documentation)

(package
 (name lang_m)
 (synopsis "Language M")
 (description "Language M")
 (depends
  ocaml
  dune
  yojson
  (ocaml-base-compiler
   (>= 5.2.0))
  ocaml-compiler-libs
  alcotest
  (core
   (>= 0.17.1))
  (core_unix
   (>= 0.17.0))
  (string_dict
   (>= 0.17.0))
  (ppx_jane
   (>= 0.17.0))
  (menhir
   (>= 3.0)))
 (tags ("Programming Language")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
