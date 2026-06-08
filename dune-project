(lang dune 3.16)

(using melange 0.1)

(name melange-transit)

(generate_opam_files true)

(package
 (name melange-transit)
 (synopsis "Transit JSON reader and writer for Melange")
 (depends
  (ocaml  (>= 5.4))
  melange
  (melange-edn (= 0.2.0))
  (melange-jest :with-test)))
