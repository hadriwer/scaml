# Changelog

SCaml follows [semantic versioning](https://semver.org/). Until 1.0, the
syntax and the standard library may still change between minor versions.

## Unreleased

### Language

- Optional arguments, as in OCaml, for `fn` (top-level and local): with a
  default value, `fn foo ?(x=0) y { .. }`, or as an option, `fn foo ?x y
  { .. }`. Passed by name, anywhere among the other arguments (`foo x=1 2`,
  `foo 2 x=1`), or left out (`foo 2`); `?x=` passes the option itself
  (`foo ?x=None 2`).

### Standard library

- `filterable` trait (`filter`) for `array` and `list`.
- `conversionnable` impls for `string` and `bool`.
- `sizable` impl for `string` (`len "abc"`).
- `maxn` and `minn`: maximum and minimum of two values.
- `compare` and `sortable` trait (`sort`) for `array`, `list` and `matrix`
  (each row sorted); `sort` returns a sorted collection and leaves arrays
  unchanged (use `Array.sort` to sort in place).
- Strings and chars are shown quoted inside a structure (list, array,
  option, tuple...), as OCaml literals: `println ["a"; "b"]` prints
  `[ "a"; "b" ]`, `println (Some 'c')` prints `Some 'c'`. On their own
  (`println "a"`), and in an impl's own `print "..."`, they stay unquoted.
- Slicing a string with a range (`s[a..b]`, `s[(a..b) + 2]`) takes linear
  time (`String.sub` for a step of 1), instead of building it char by char.

### Fixed

- Char literals accept any character and OCaml's escapes (`' '`, `'1'`,
  `'\n'`, `'\''`...), not only a letter: `String.split_on_char ' '` failed
  with "Unbound value '" (#6).
- `!=` is now the negation of `==` (structural), not OCaml's physical
  inequality: `"a" != "a"` was `true`.
- A trait method called on a value whose type nothing fixes, in code that
  can never run, no longer fails with "No impl of ...": e.g. `x + v` in
  `match u { Some x -> x + v | None -> .. }` when `u` is an optional
  argument left out (or `None`).

## 0.1.0 (2026-10-12)

First tagged version.

### Language

- Functions (`fn`), user-defined operators (`op`), anonymous functions
  (`^x -> ...`), local functions, recursion.
- Operator precedence as in OCaml (`*` before `+`).
- Records, variants, pattern matching (`match`), tuples.
- Traits (`trait`) and their implementations (`impl ... of ...`), resolved at
  compile time (modular implicits), including impls over two types
  (`impl arithm of int, float`) and over parametric types (`array`, `list`, ...).
- Ranges `a..b` and `a..=b`, with a step (`(0..=10) + 2`), and range
  destructuring in `let` (`let a, b, c = (0..3)`).
- Indexing and slicing of arrays and strings: `a[i]`, `m[i, j]`, `a[1..3]`,
  `a[(0..5) + 2]`.
- Exceptions: `try ... with { | ... }`.
- `#use "file.scaml"` to include another file.

### Standard library

- Traits: `showable` (`print`, `println`), `arithm` (`+ - * / %`),
  `conversionnable` (`to_int`, `to_float`, `to_string`), `iterable`,
  `foldable`, `mappable`, `quantifiable` (`forall`, `exists`), `sizable`
  (`len`), `concetanable` (`@`), `indexable` (`[]`), `negatable`,
  `extremable` (`max`, `min`, `argmax`, `argmin`).
- Implementations for `int`, `float`, `string`, `char`, `bool`, `unit`,
  `array`, `list`, `option`, tuples, ranges and binary trees.
- `stdlib/matrix.scaml`: matrices, `transpose`, `trace`, product `*.`,
  eigenvalues (`eig`).
- `stdlib/complex.scaml`: complex numbers, `eig_complex`.

### Compiler

- `scamlc file.scaml` compiles to a native executable through OCaml;
  `--keep` keeps the generated OCaml, `--verbose` prints it,
  `--check-stdlib` type-checks the whole standard library.
- Type errors reported on the SCaml source, including missing trait impls
  (`` No impl of `[]` (trait `indexable`) for list, int ``).
- Only the part of the standard library a program uses is type-checked, and
  chains of trait calls are resolved in fewer rounds: 11 to 34% faster
  compilation than without these optimizations.
- Test suite: `dune test`.

### Editors

- Syntax highlighting for VS Code and Neovim (`editors/`).

### Fixed

- `string * int` repeated the string one time too many.
