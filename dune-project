(lang dune 3.16)

(using melange 0.1)

(name melange-transit)
(version 0.1.0)

(generate_opam_files true)

(package
 (name melange-transit)
 (synopsis "Transit JSON reader and writer for Melange")
 (description "melange-transit reads and writes Transit JSON values for Melange.")
 (maintainers "rcmerci <rcmerci@gmail.com>")
 (authors "rcmerci")
 (license MIT)
 (homepage "https://github.com/RCmerci/melange-transit")
 (bug_reports "https://github.com/RCmerci/melange-transit/issues")
 (source (github RCmerci/melange-transit))
 (depends
  (ocaml  (>= 5.2))
  melange
  (melange-edn (= 0.5.0))
  (melange-jest :with-test)))
