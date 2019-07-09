(lang dune 1.10)
(using fmt 1.1)
(implicit_transitive_deps false)
(generate_opam_files true)

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
    (dune (and :build (>= 1.10)))
    (odoc :build)
    (cmdliner (>= 1.0.4)))
  (depopts
    junit
    patience_diff
    qcheck-core))
