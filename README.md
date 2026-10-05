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

```bash
dune build
dune exec bin/main.exe -- examples/hello.scaml
./examples/hello.exe
```

Use `--keep` (`-k`) to keep the generated OCaml source and intermediate files.

## Editor support

A VS Code extension (syntax highlighting, file icon) is available in
[`editors/vscode`](editors/vscode).

## License

[MIT](LICENSE) © 2026 wer. SCaml uses OCaml's `compiler-libs` (LGPL 2.1 with
linking exception); `reference/ast_helper.ml` remains under OCaml's license.
