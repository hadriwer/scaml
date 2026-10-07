# SCaml documentation

Welcome to the SCaml documentation. SCaml (Simple Caml) is a functional
language with a lightweight, Rust-inspired syntax that compiles to OCaml.

## Table of contents

0. [Introduction](00_introduction.md): why SCaml exists, and how traits give
   you one `+`, one `print` and one `fold_left` for every type.
1. [Functions and operators](01_functions.md): defining functions and
   operators, your first program, recursion and anonymous functions.
2. [Loops](02_loops.md): ranges, `iter`, and building your own `for`.

## Running the examples

Every snippet in these pages is a complete program unless stated otherwise.
Save it as `hello.scaml`, then from the repository root:

```bash
dune exec bin/main.exe -- hello.scaml
./hello.exe
```

---

[Next: Introduction →](00_introduction.md)

<sub>© 2026 wer · SCaml is released under the [MIT License](../LICENSE).</sub>
