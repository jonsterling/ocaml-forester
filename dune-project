(lang dune 3.7)

(using menhir 2.0)

(name forester)

(authors "Jonathan Sterling")

(maintainers "Jonathan Sterling")

(version 2.4)

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
  (ocamlgraph
   (>= 2.0.0))
  (ocaml
   (>= 0.5.0))
  dune
  (xmlm
   (>= 1.4.0))
  ppx_deriving
  (cmdliner
   (>= 1.2.0))
  dune-build-info
  (domainslib
   (>= 0.5.0))
  (eio_main
   (>= 0.12))
  (ptime
   (>= 1.1.0))
  (yuujinchou
   (>= 5.0.1))
  (algaeff
   (>= 0.2.1))))
