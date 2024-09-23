(lang dune 3.15)

(using menhir 3.0)

(name lang_m)

(generate_opam_files true)

; (source
;  (github username/reponame))

(authors "Jay Lee")

(maintainers "Jay Lee")

(license LICENSE)

; (documentation https://url/to/documentation)

(package
 (name lang_m)
 (synopsis "Language M")
 (description "Language M")
 (depends
  (core
   (>= 0.17.1))
  (core_unix
   (>= 0.17.0))
  (string_dict
   (>= 0.17.0))
  (ppx_jane
   (>= 0.17.0))
  (menhir
   (>= 20231231)))
 (tags ("Programming Language")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
