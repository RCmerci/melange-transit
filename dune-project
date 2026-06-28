(lang dune 3.16)

(using melange 0.1)

(name melange-transit)
(version 0.1.0)

(generate_opam_files true)

(package
 (name melange-transit-core)
 (synopsis "Platform-independent Transit JSON types and EDN conversion")
 (description
  "melange-transit-core provides shared Transit JSON types and EDN conversion.")
 (maintainers "rcmerci <rcmerci@gmail.com>")
 (authors "rcmerci")
 (license MIT)
 (homepage "https://github.com/RCmerci/melange-transit")
 (bug_reports "https://github.com/RCmerci/melange-transit/issues")
 (source (github RCmerci/melange-transit))
 (depends
  (ocaml  (>= 5.2))
  (melange-edn-core (= 0.5.0))))

(package
 (name melange-transit-melange)
 (synopsis "Transit JSON reader and writer for Melange")
 (description "melange-transit-melange reads and writes Transit JSON values on Melange.")
 (maintainers "rcmerci <rcmerci@gmail.com>")
 (authors "rcmerci")
 (license MIT)
 (homepage "https://github.com/RCmerci/melange-transit")
 (bug_reports "https://github.com/RCmerci/melange-transit/issues")
 (source (github RCmerci/melange-transit))
 (depends
  (ocaml  (>= 5.2))
  melange
  (melange-transit-core (= :version))
  (qcheck :with-test)))

(package
 (name melange-transit-jsoo)
 (synopsis "Transit JSON reader and writer for js_of_ocaml")
 (description "melange-transit-jsoo reads and writes Transit JSON values on js_of_ocaml.")
 (maintainers "rcmerci <rcmerci@gmail.com>")
 (authors "rcmerci")
 (license MIT)
 (homepage "https://github.com/RCmerci/melange-transit")
 (bug_reports "https://github.com/RCmerci/melange-transit/issues")
 (source (github RCmerci/melange-transit))
 (depends
  (ocaml  (>= 5.2))
  js_of_ocaml
  (melange-transit-core (= :version))
  (qcheck :with-test)))

(package
 (name melange-transit-native)
 (synopsis "Transit JSON reader and writer for native OCaml")
 (description "melange-transit-native reads and writes Transit JSON values on native OCaml.")
 (maintainers "rcmerci <rcmerci@gmail.com>")
 (authors "rcmerci")
 (license MIT)
 (homepage "https://github.com/RCmerci/melange-transit")
 (bug_reports "https://github.com/RCmerci/melange-transit/issues")
 (source (github RCmerci/melange-transit))
 (depends
  (ocaml  (>= 5.2))
  (melange-transit-core (= :version))
  ptime
  (qcheck :with-test)
  yojson))
