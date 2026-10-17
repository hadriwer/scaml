# Loops

SCaml is a functional language (sorry, fans of imperative programming): there
is **no `while` loop and no `for` loop**. Repetition is done with recursion,
or with higher-order functions such as `iter`, `map` and `fold_left` over
**ranges** and collections.

## Ranges

A range is a sequence of integers, written with `..` (end excluded) or `..=`
(end included):

```rust
(0..10)         // 0 to 9
(0..=10)        // 0 to 10
(10..0)         // 10 down to 1
(10..=0)        // 10 down to 0
```

The direction follows the bounds: if the start is greater than the end, the
range counts down.

Adding or subtracting an integer sets the **step**:

```rust
(0..=10) + 2        // 0, 2, 4, 6, 8, 10
(10..=0) - 2        // 10, 8, 6, 4, 2, 0
```

Ranges are not built into the language. The standard library defines them as
an ordinary type with two operators:

```rust
type range {
    | Range of int, int, (int -> int -> bool)
    | RangeStep of int, int, (int -> int -> bool), int
}

op .. lo hi { if lo <= hi then Range (lo, hi, (<)) else Range (lo, hi, (>)) }

op ..= lo hi { if lo <= hi then Range (lo, hi, (<=)) else Range (lo, hi, (>=)) }
```

Ranges implement the same traits as lists and arrays (`iter`, `fold_left`,
`forall`, `exists`, ...), so they work with the same functions.

### Slicing with a range

Indexing a string or an array with a range, instead of an integer, takes
the part it covers, step and direction included:

```rust
fn main () {
    let s = "abcdefgh";
    println s[2..5];            // cde
    println s[2..=5];           // cdef
    println s[7..=0];           // hgfedcba (reversed)
    println s[(0..8) + 2];      // aceg

    let a = [|10; 20; 30; 40; 50|];
    println a[1..3];            // [| 20; 30 |]
    println a[(4..=0) - 2];     // [| 50; 30; 10 |]
}
```

A slice is built in one go, in time proportional to its length: for a
string with a step of 1, `s[a..b]` is exactly OCaml's `String.sub s a (b -
a)`, so it can be used freely, e.g. to cut a string into pieces in a
loop. A range reaching past the end of the string or the array is an
error at run time (`Invalid_argument`), and a step of `0` gives an empty
slice.

## Looping with `iter`

`iter f c` calls `f` on every element of `c`, in order:

```rust
fn main () {
    iter println (0..=10);          // prints every number from 0 to 10
    iter println [|"a"; "b"|];      // works on arrays and lists too
}
```

## Build your own `for`

Missing your `for` loop? Write it yourself! Because `iter` works on any
iterable, so does your `for`:

```rust
fn for iterator body {
    iter body iterator;
}

fn main () {
    let even = ^x -> x % 2 == 0;

    for (0..4) (^i -> {
        if even i then println "even" else println "odd"
    });

    for [1; 2; 3; 4] (^i -> {
        if even i then println "even" else println "odd"
    });
}
```

---

[← Functions and operators](01_functions.md) · [Index](README.md)

<sub>© 2026 wer · SCaml is released under the [MIT License](../LICENSE).</sub>
