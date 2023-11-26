(lang dune 3.11)

(name ocaml_vcd)

(generate_opam_files true)
(map_workspace_root false)

(source
 (github username/reponame))

(authors "LavovaLampa")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name ocaml_vcd)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
