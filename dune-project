(lang dune 3.7)

(using menhir 2.0)

(name forester)

(authors "Jonathan Sterling")

(maintainers "Jonathan Sterling")

(version 2.2)

(generate_opam_files true)

(source
 (sourcehut jonsterling/ocaml-forester))

(bug_reports https://todo.sr.ht/~jonsterling/forester)

(homepage https://sr.ht/~jonsterling/forester/)

(license GPL-3.0-or-later)

(package
 (name forester)
 (synopsis "A tool for tending mathematical forests")
 (depends
  menhir
  ocamlgraph
  ocaml
  dune
  xmlm
  ppx_deriving
  domainslib
  ptime
  (yuujinchou
   (>= 5.0.1))
  algaeff))
