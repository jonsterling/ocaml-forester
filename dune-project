(lang dune 3.7)

(using menhir 2.0)

(name forester)

(authors "Jonathan Sterling")

(maintainers "Jonathan Sterling")

(version 4.0.0)

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
  (menhir
   (>= 20211230))
  (ocamlgraph
   (>= 2.1.0))
  (ocaml
   (>= 5.0.0))
  dune
  ppx_deriving
  (cmdliner
   (>= 1.2.0))
  dune-build-info
  (uucp
   (>= 15.1.0))
  (eio_main
   (>= 1.0))
  (ptime
   (>= 1.1.0))
  (asai
   (>= 0.2.0))
  (yuujinchou
   (>= 5.2.0))
  (bwd
   (>= 2.3.0))
  (algaeff
   (>= 2.0.0))
  (yojson
   (>= 2.1.2))
  (dream-html
   (>= 3.3.1))
  (toml
   (>= 7.1.0))))
