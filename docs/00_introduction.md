# Introduction

## Why SCaml?

Thank you for your interest in SCaml!

I love how OCaml works, but I find its syntax heavy. SCaml keeps OCaml's
semantics, type system and performance, and puts a more modern,
easy-to-use syntax in front of them, inspired by Rust.

SCaml is a good fit for writing algorithms quickly while keeping good
performance, for proofs of concept or for competitive-programming-style code.

Under the hood, a SCaml program is parsed straight into OCaml's own AST,
then type-checked and compiled by the OCaml toolchain:

```
SCaml → OCaml → native binary
```

## Everything is a trait

The core idea of SCaml is that **the whole language is built by defining
traits for operators and functions**. Even `+` and `println` are not built in:
they are declared in the standard library as trait methods, then implemented
for each type.

> [!TIP]
> The best way to understand the language is to read
> [`stdlib/core.scaml`](../stdlib/core.scaml): it is written in SCaml itself.

Here is how the standard library declares printing and arithmetic:

```rust
trait showable {
    type a
    fn print of a -> unit
    fn println of a -> unit
}
```

```rust
trait arithm {
    op + of a -> b -> c
    op - of a -> b -> c
    op * of a -> b -> c
    op / of a -> b -> c
    op % of a -> b -> c
}
```

Each type then gets its own implementation:

```rust
impl showable of int {
    fn print i { scaml_print_int i }
    fn println i { print i; scaml_print_newline () }
}
```

## Modular implicits

Before compiling, SCaml checks the type of every expression and picks the
right implementation (the right OCaml module) for each trait call. This is
known as **modular implicits**: the same operator or function name works on
every type that implements it.

Compare the same program in OCaml and in SCaml:

```ocaml
(* OCaml *)
let () =
  print_int (1 + 2);
  print_newline ();
  print_float (1. +. 2.);
  print_newline ();

  let l = List.fold_left (+) 0 [1; 2; 3] in
  let l' = Array.fold_left (+.) 0. [|1.; 2.; 3.|] in

  print_int l;
  print_newline ();
  print_float l';
  print_newline ()
```

```rust
// SCaml
fn total acc c {
    fold_left (+) acc c
}

fn main {
    println (1 + 2);
    println (1. + 2.);

    let l = total 0 [1; 2; 3];
    let l' = total 0. [|1.; 2.; 3.|];

    println l;
    println l';
}
```

In SCaml there is a single `+` for `int` and `float`, a single `println` for
every printable type, and a single `fold_left` for lists and arrays. Even
`total`, written once, works on both.

> [!NOTE]
> The standard library already provides this function as `sum`.

---

[← Index](README.md) · [Next: Functions and operators →](01_functions.md)

<sub>© 2026 wer · SCaml is released under the [MIT License](../LICENSE).</sub>
