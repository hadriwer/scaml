(lang dune 3.21)

(using menhir 3.0)

(name SCaml)

(version 0.2.0)

(generate_opam_files true)

(source
 (github hadriwer/scaml))

(authors "wer")

(maintainers "wer")

(license MIT)

(documentation https://github.com/hadriwer/scaml/tree/main/docs)

(package
 (name SCaml)
 (synopsis "A functional language with a Rust-inspired syntax that compiles to OCaml")
 (description
  "SCaml (Simple Caml) is a functional language with a lightweight, Rust-inspired syntax. Its operators and functions are trait methods, resolved at compile time (modular implicits). SCaml is parsed into OCaml's own AST, type-checked and compiled to a native binary with the OCaml toolchain.")
 (depends
  (ocaml
   (>= 5.5))
  (menhir :build)
  ocamlfind)
 (tags
  (language compiler traits "modular implicits")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
