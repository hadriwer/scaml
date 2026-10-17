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

fn main () {
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

## Impls on nested types

An impl usually targets a type, `impl showable of array`, for every array.
It can also target a **nested type**, written like a type, its parameters
named: `e array array` (an array of arrays of anything), `int array` (an
array of `int`s only), `(k, string) Hashtbl.t`. For a call, the **most
specific** impl that applies is used: the one naming the most types.

The standard library's `to_matrix` ([`stdlib/matrix.scaml`](../stdlib/matrix.scaml))
is defined this way:

```rust
trait conversionnable_matrix {
    type b a
    fn to_matrix of a -> b matrix
}

// a vector: a column
impl conversionnable_matrix of e array {
    type b = e
    fn to_matrix v { { arr = Array.map (^x -> [| x |]) v; shape = (len v, 1) } }
}

// rows: more specific, so used for an array of arrays
impl conversionnable_matrix of e array array {
    type b = e
    fn to_matrix rows {
        let cols = if len rows == 0 then 0 else len rows[0];
        { arr = rows; shape = (len rows, cols) }
    }
}
```

```rust
#use "stdlib/matrix.scaml"

fn main () {
    println (to_matrix [| 1; 2; 3 |]).shape;                  // (3, 1)
    println (to_matrix [| [| 1; 2 |]; [| 3; 4 |] |]).shape;   // (2, 2)
}
```

A few rules:

- The impl gives the trait's other types in terms of its parameters
  (`type b = e`).
- Two impls as specific as each other that both apply are an error
  (`(int, v) Hashtbl.t` and `(k, string) Hashtbl.t`, for an `(int, string)
  Hashtbl.t`).
- Inside generic code, where a type is only partly known (an impl on `a
  grid` printing its `a array array`), an impl that needs a precise type
  there (`float array array`) can't be told to apply: the more general one
  is used. One whose pattern only has parameters there (`e array array`)
  is.
- Not for a trait on a type constructor (`type t of 1`, like `mappable`),
  nor for an overloaded impl (`impl arithm of array, int`).

---

[← Index](README.md) · [Next: Functions and operators →](01_functions.md)

<sub>© 2026 wer · SCaml is released under the [MIT License](../LICENSE).</sub>
