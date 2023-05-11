(lang dune 3.7)

(using menhir 2.0)

(name forester)

(generate_opam_files true)

(source
 (github jonsterling/ocaml-forester))

(authors "Jonathan Sterling")

(maintainers "Jonathan Sterling")

(license GPL-3.0-or-later)

(package
 (name forester)
 (synopsis "A tool for tending mathematical forests")
 (depends menhir ocamlgraph ocaml dune xmlm ppx_deriving domainslib))
