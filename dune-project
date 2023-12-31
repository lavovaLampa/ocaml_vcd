(lang dune 3.12)

(name ocaml_vcd)
(version 0.0.1)

(generate_opam_files true)
(map_workspace_root false)

(source
 (github lavovaLampa/ocaml_vcd))

(authors "LavovaLampa <lavovalampa@protonmail.com>")

(maintainers "LavovaLampa <lavovalampa@protonmail.com>")

(license MIT)

(documentation https://url/to/documentation)

(package
 (name ocaml_vcd)
 (synopsis "Value Change Dump parser")
 (description "Pure OCaml VCD file parser (as defined in IEEE 1364-1995)")
 (depends ocaml dune sedlex ppx_show uutf)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
