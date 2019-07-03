(lang dune 1.10)
(using fmt 1.1)
(implicit_transitive_deps false)

(generate_opam_files true)
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
    ; Build-time (:build)
    (dune (and :build (>= 1.10)))
    (odoc :build)
    ; Runtime
    lwt
    lwt_ppx
    react
    cmdliner
    patience_diff))
