(lang dune 3.7)

(using menhir 2.0)

(name entmoot)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Jonathan Sterling")

(maintainers "Jonathan Sterling")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name entmoot)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune xmlm ppx_deriving shexp)
 (tags
  (topics "to describe" your project)))
