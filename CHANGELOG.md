# Changelog

SCaml follows [semantic versioning](https://semver.org/). Until 1.0, the
syntax and the standard library may still change between minor versions.

## Unreleased

### Language

- An impl can name its type's parameters, to give the trait's types in
  terms of them: `impl iterable of (k, v) Hashtbl.t { type a = (k, v) ..
  }`. Such an impl can use a trait on each parameter (`showable of (k, v)
  Hashtbl.t` printing keys and values): impls of types with several
  parameters failed with "Unbound type constructor X.__elem1__".
- `_` in an overloaded impl's types stands for any type: `impl indexable
  of Hashtbl.t, _ { .. }`, for every key type (tuples included). An impl
  naming the type (`impl indexable of Hashtbl.t, int`) takes precedence.
- Optional arguments, as in OCaml, for `fn` (top-level and local): with a
  default value, `fn foo ?(x=0) y { .. }`, or as an option, `fn foo ?x y
  { .. }`. Passed by name, anywhere among the other arguments (`foo x=1 2`,
  `foo 2 x=1`), or left out (`foo 2`); `?x=` passes the option itself
  (`foo ?x=None 2`).
- Parameters are patterns, in `fn`, `op`, impl methods and lambdas: a tuple
  is destructured, `fn foo (x, y) { .. }`, `^(k, v) -> ..`, also nested
  (`((a, b), c)`), with `_` for an unused one; a record in parentheses,
  `fn norm ({ x; y }) { .. }` (#14). `fn foo (a, b)` used to declare two
  parameters `a` and `b`: write `fn foo a b`.
- Indexing operators with any suffix after `[]`, not only `[]=`: `op []+=
  a i v { .. }` is used as `a[i] += v` (#10). They take 3 arguments, and
  `a[i] <op> v` is an expression (`a[i] = v` too), whose value `v` extends
  as far as it can. A literal negative number (`-1`) can't be a parameter
  anymore (it still can in `match` and `let`).
- Constants: a `let` outside any function, `let pi = 3.14;`, with any
  pattern (`let (w, h) = (800, 600);`). The name is lowercase.

### Standard library

- `filterable` trait (`filter`) for `array` and `list`.
- `conversionnable` impls for `string` and `bool`.
- `to_char` in `conversionnable`: the char of a code (`to_char 97` is
  `'a'`), of a one-char string or bytes, `to_char (Some x)`; fails on the
  types without one (`bool`, `list`...).
- `sizable` impl for `string` (`len "abc"`).
- `maxn` and `minn`: maximum and minimum of two values.
- `compare` and `sortable` trait (`sort`) for `array`, `list` and `matrix`
  (each row sorted); `sort` returns a sorted collection and leaves arrays
  unchanged (use `Array.sort` to sort in place).
- Strings and chars are shown quoted inside a structure (list, array,
  option, tuple...), as OCaml literals: `println ["a"; "b"]` prints
  `[ "a"; "b" ]`, `println (Some 'c')` prints `Some 'c'`. On their own
  (`println "a"`), and in an impl's own `print "..."`, they stay unquoted.
- `[]+=` in the `indexable` trait, for arrays (`a[i] += v`) and matrices
  (`m[i] += row` adds `row` to the row `i`).
- `stdlib/math.scaml` (`#use "stdlib/math.scaml"`): the constant `pi`,
  and `relu`, `leaky_relu` (optional `alpha`, `0.01` by default),
  `sigmoid`, `tanh`, `softmax` (on a float array), and `is_close a b`,
  float equality up to rounding errors like Python's `math.isclose`
  (optional `rel_tol` and `abs_tol`, both `1e-9` by default, so that
  `sin pi |> is_close 0.` holds).
- Slicing a string with a range (`s[a..b]`, `s[(a..b) + 2]`) takes linear
  time (`String.sub` for a step of 1), instead of building it char by char.
- `stdlib/hashmap.scaml` (`#use "stdlib/hashmap.scaml"`): hash maps
  (`Hashtbl.t`), `hashmap_of_list`, `get`, `insert`, `update`, `keys`,
  `entries`..., `hm[k]`, `hm[k] = v`, `hm[k] += v` for any key type,
  `hm1 @ hm2`, `len`, `println hm`, and `iter`, `fold_left`, `forall`...
  over its `(key, value)` bindings.
- `stdlib/dfa.scaml` (`#use "stdlib/dfa.scaml"`): deterministic finite
  automata, with states of any type: `init_dfa q0 finals transitions`,
  `match_pattern`, `next_state`, `is_final`, `println`, and `determinize`
  (subset construction) from nondeterministic transitions, `""` standing
  for an ε-transition (`init_dfa` refuses them).

### Compiler

- `scamlc --version` prints the compiler version.
- Faster compilation (8 to 25 % on the test programs): each round of
  trait resolution only walks again the code that changed, and chains of
  overloaded calls (`t[i][j]`, `h[k] + 1`...) are resolved in one round
  instead of one per call.

### Editors

- VS Code and Neovim: function calls have their own color, told from
  variables by their position (the head of an application, `println x`,
  is a function, its arguments aren't), and so do the standard library's
  functions passed as values; types after `of` are colored as types.
  Parameters of indexing operators (`op []+= a i v`) are no longer taken
  for something else.

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
- Types from OCaml modules can be written where a type is expected:
  `impl sizable of Hashtbl.t`, `impl indexable of Hashtbl.t, string`, and
  in a type, with several parameters as in OCaml: `W of (string, int)
  Hashtbl.t` (#15).
- A string or a char inside a structure is quoted by `println` too, not
  only by `print`: inside a generic impl, `println x` on an element showed
  `a` where `print x` showed `"a"`.
- Inside a generic impl, `println` on an array (or a list...) of its own
  element type failed with "Modules do not match".
- A local function calling another generic local function inside a
  trait's callback (`fn h i { map (^e -> g i) l }`, `g` indexing with
  `p[i]`) failed with "Unbound value `[]`".
- The error for a call that doesn't type-check inside a nested function
  showed mangled types (`string] [get_trans : string -> _`).
- No more "unused variable" warning from OCaml for a local function
  whose specialized copies replaced it.

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
