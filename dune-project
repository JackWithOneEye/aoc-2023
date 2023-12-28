(lang dune 3.10)

(name aoc_2023)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name aoc_2023)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
  (ocaml
   (>= "5.0"))
  core
  core_kernel
  dune
  dream
  dream-livereload
  ppx_deriving
  ppx_jane
  reason
  tyxml
  tyxml-jsx)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
