(lang dune 3.7)

(using menhir 2.0)

(name entmoot)

(generate_opam_files true)

(source
 (sourcehut jonsterling/entmoot))

(authors "Jonathan Sterling")

(maintainers "Jonathan Sterling")

(license GPL-3.0-or-later)

(package
 (name entmoot)
 (synopsis "A tool for tending mathematical forests")
 (depends ocaml dune xmlm ppx_deriving domainslib))
