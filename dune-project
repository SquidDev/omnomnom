(lang dune 2.0)
(implicit_transitive_deps false)
(generate_opam_files true)

(name omnomnom)
(version 0.1.0)
(authors "Jonathan Coates")
(license MIT)
(source (github squiddev/omnomnom))
(maintainers "Jonathan Coates <jonathan.j.coates@outlook.com>")

(package
  (name omnomnom)
  (synopsis "Another tiny test framework")
  (description "A small test framework, inspired by Tasty.")
  (depends
    (ocaml (>= 4.08))
    (dune (>= 2.0))
    (odoc :with-doc)
    (cmdliner (>= 1.0.4))
    (mtime (>= 2.0.0))
    (junit (and :with-test (>= 2.0.0)))
    (alcotest (and :with-test (>= 1.2.0)))
    (qcheck-core (and :with-test (>= 0.10))))
  (depopts
    (junit (>= 2.0.0))
    (alcotest (>= 1.2.0))
    (qcheck-core (>= 0.10))))
