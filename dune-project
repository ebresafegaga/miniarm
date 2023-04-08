(lang dune 3.0)

(name miniarm)

(generate_opam_files true)

(source
 (github ebresafegaga/miniarm))

(authors "Oghenevwogaga Ebresafe")

(maintainers "Oghenevwogaga Ebresafe")

(license LICENSE)

(documentation https://github.com/ebresafegaga/miniarm)

(using menhir 2.1)

(package
 (name miniarm)
 (synopsis "A simple arm emulator")
 (description "A simple arm emulator")
 (depends
  (ocaml
   (>= 4.13))
  dune
  menhir
  ppx_deriving)
 (tags
  (emulator abstract-machine)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
