(lang dune 3.21)

(using menhir 3.0)

(name SCaml)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "wer")

(maintainers "wer")

(license MIT)

(documentation https://url/to/documentation)

(package
 (name SCaml)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml)
 (tags
  ("add topics" "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
