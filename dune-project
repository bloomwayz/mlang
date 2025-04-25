(lang dune 3.15)

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
  base
  stdio
  yojson
  ocaml-compiler-libs
  (string_dict
   (>= 0.17.0))
  yojson
  ppx_yojson_conv
  (alcotest :with-test))
 (tags ("Programming Language")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
