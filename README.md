<p align="center">
  <img src="scaml_logo.jpeg" alt="SCaml logo" width="400">
</p>

# SCaml

**SCaml** (Simple Caml) is a functional language with a lightweight,
Rust-inspired syntax that compiles to OCaml.

```
fn length l {
    fold_left (^acc e -> 1 + acc) 0 l
}

fn main {
    println (length [1; 2; 3]);
}
```

SCaml is parsed directly into OCaml's own AST, then type-checked and compiled
with the OCaml toolchain: `SCaml → OCaml → native binary`.

## Requirements

- OCaml 5.5
- dune, Menhir, ocamlfind

## Usage

Install the `scamlc` compiler once, from the repository root:

```bash
dune build
dune install
```

This copies `scamlc` into your opam switch's `bin/` directory, so you can then
compile and run a program from any directory on your machine:

```bash
scamlc hello.scaml
./hello.exe
```

The standard library is embedded in the binary, so `scamlc` does not need the
repository to be present. It does need your opam switch to be active in the
shell (`eval $(opam env)`, usually added to your shell config by `opam init`):
that is what puts `scamlc` on your `PATH`, and `scamlc` calls
`ocamlfind ocamlopt` from the same switch to build the executable.

The installed `scamlc` is a copy: after changing the compiler, re-run
`dune build && dune install` from the repository root. During
development you can also skip the install with
`dune exec scamlc -- examples/hello.scaml`.

Use `--keep` (`-k`) to keep the generated OCaml source and intermediate files.

## Editor support

A VS Code extension (syntax highlighting, file icon) is available in
[`editors/vscode`](editors/vscode).

## License

[MIT](LICENSE) © 2026 wer. SCaml uses OCaml's `compiler-libs` (LGPL 2.1 with
linking exception); `reference/ast_helper.ml` remains under OCaml's license.
