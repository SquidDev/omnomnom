(lang dune 1.10)
(using fmt 1.1)
(implicit_transitive_deps false)
(generate_opam_files true)

(version 0.1.0)
(authors "SquidDev")
(license MIT)
(source (github squiddev/omnomnom))
(maintainers "Jonathan Coates")

(package
  (name omnomnom)
  (synopsis "Another tiny test framework")
  (description "A small test framework, inspired by Tasty.")
  (depends
    (ocaml (>= 4.08))
    (dune (and :build (>= 1.10)))
    (odoc :build)
    (lwt  (>= 4.2.1))
    (lwt_ppx (>= 1.2.2))
    (cmdliner (>= 1.0.4))))
