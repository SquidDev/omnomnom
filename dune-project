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

(package
  (name omnomnom-golden)
  (synopsis "Golden tests for Omnomnom.")
  (description "Runs a function against the contents of an input file, and compares the output to a \"golden\" file, containing the expected output.")
  (depends
    (ocaml (>= 4.08))
    (dune (and :build (>= 1.10)))
    (odoc :build)
    omnomnom
    (lwt  (>= 4.2.1))
    (lwt_ppx (>= 1.2.2))
    (cmdliner (>= 1.0.4))
    (omnomnom (>= ${version:omnomnom}))
    (patience_diff (>= 0.12.0))))

(package
  (name omnomnom-junit)
  (synopsis "JUnit test reporter for Omnomnom.")
  (description "A test reporter for Omnomnom which produces JUnit XML files, suitable for use with CI systems such as Jenkins or Azure.")
  (depends
    (ocaml (>= 4.08))
    (dune (and :build (>= 1.10)))
    (odoc :build)
    omnomnom
    (lwt  (>= 4.2.1))
    (lwt_ppx (>= 1.2.2))
    (cmdliner (>= 1.0.4))
    (omnomnom (>= ${version:omnomnom}))
    (junit (>= 2.0.1))))
