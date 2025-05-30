(lang dune 3.17)

(using menhir 3.0)

(name lang_m)

(generate_opam_files true)

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
  menhir
  dune
  ocaml-compiler-libs
  yojson
  ppx_yojson_conv)
 (tags ("Programming Language")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
