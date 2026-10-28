# Functions and operators

## Defining functions and operators

Functions and operators share the same definition syntax:

```text
fn <name> <args>* { <body> }
op <symbols> <arg1> <arg2> { <body> }                 // a <symbols> b
op [] <arg1> <arg2> { <body> }                        // a[i]
op []? <arg1> <arg2> { <body> }                       // a[i]?
op []<symbols> <arg1> <arg2> <arg3> { <body> }        // a[i] <symbols> v
```

The only difference is how they are called. A function is **prefix** (its
name comes first), while an operator, made of symbols, is **infix** (it goes
between its two arguments). An indexing operator, starting with `[]`, takes
the container and the index, plus the value for one with symbols after the
`[]`: three arguments (see [Indexing operators](#indexing-operators)).
Wrapping an operator in parentheses turns it back into a prefix function:

```rust
fn foo a b { print a; print " << "; println b; }

op << a b { print a; print " << "; println b; }

fn main () {
    foo "a" 1;      // a << 1

    1 << "a";       // 1 << a
    (<<) 1 "a";     // 1 << a
}
```

A few rules:

- The entry point of a program is the `main` function, which takes `()`:
  `fn main () { .. }`.
- A function that takes nothing takes `()`, and says so: `fn hello () {
  .. }`, called as `hello ()`. Without any parameter, a `fn` must be a
  function itself (see [η-reduction](#η-reduction)).
- The last expression of a block is its return value. No `return` keyword
  is needed.
- An operator takes exactly two arguments, except an indexing one with
  symbols after its `[]` (`[]=`, `[]+=`...), which takes exactly three
  (`[]?` takes two).

### Destructuring parameters

A parameter can be a pattern, as in `let` or `match`: a tuple is taken
apart right in the parameter list. This works for functions, operators,
impl methods and anonymous functions:

```rust
type point { x of int; y of int }

fn add (x, y) { x + y }
fn area ((x1, y1), (x2, y2)) { (x2 - x1) * (y2 - y1) }
fn second _ y { y }                     // `_`: a parameter not used
fn norm1 ({ x; y }) { abs x + abs y }   // a record, in parentheses

op <+> (a, b) (c, d) { (a + c, b + d) }

fn main () {
    println (add (1, 2));                       // 3
    println (area ((0, 0), (2, 3)));            // 6
    println (second "ignored" 5);               // 5
    println (norm1 { x = 3; y = 0 - 4 });       // 7
    println ((1, 2) <+> (10, 20));              // (11, 22)
    println (map (^(k, v) -> k * v) [(1, 2); (3, 4)]);  // [ 2; 12 ]
}
```

`fn add (x, y)` takes **one** parameter, a pair, called as `add (1, 2)`.
For two parameters, write `fn add x y`, called as `add 1 2`.

A record needs parentheses around it, `({ x; y })`: a bare `{` after the
parameters starts the function's body.

## Imperative style

The whole language can also be written in a C-like style, with parentheses
around a function's parameters and a call's arguments:

```rust
fn foo(x, y) {
    println("x = " + to_string(x));
    println("y = " + to_string(y));
    println("x + y = " + to_string(x + y));
}

fn hello() { println("hello"); }

fn main() {
    foo(2, 3);                  // x = 2, y = 3, x + y = 5
    hello();                    // hello
}
```

There is no special syntax behind it: `fn foo(x, y)` takes a single
parameter, a pair, destructured (see
[Destructuring parameters](#destructuring-parameters)), and `foo(2, 3)`
passes it the pair `(2, 3)`. Likewise `fn hello()` takes `unit`, and
`hello()` passes it `()`. A single parameter in parentheses, `fn
square(x)`, is just `x`.

So both styles can be mixed, as long as a function is called the way it
is defined. The standard library is written in the functional style, its
functions take their arguments one by one: `map(square, l)` would pass
`map` a pair, write `map square l` or `map(square)(l)` instead.

```rust
fn square(x) { x * x }

fn main() {
    println(square(5));                 // 25
    println(map square [1; 2; 3]);      // [ 1; 4; 9 ]
    println(map(square)([1; 2; 3]));    // [ 1; 4; 9 ]
}
```

## Your first program

```rust
fn main () {
    println "Hello, World!"
}
```

And your first operator, the sum of absolute values:

```rust
op |+| x y {
    (abs x) + (abs y)
}

fn main () {
    println (-1 |+| -2)     // 3
}
```

An operator's precedence comes from its first character, as in OCaml. `|+|`
starts with `|`, so it binds like a comparison, while an operator starting
with `*` binds like a multiplication.

### Bitwise operators

On ints, the standard library has the bitwise operators, written with three
characters (a lone `|`, `^` or `&` are already taken: a pattern's `|`, a
lambda's `^`, the boolean `&&`):

| Operator | Meaning | Priority, as OCaml's |
|---|---|---|
| `a &! b` | and | `land`, like `*` |
| `a \|\|\| b` | or | `lor`, like `*` |
| `a ^! b` | xor | `lxor`, like `*` |
| `a <! n` | shift left | `lsl`, like `**` |
| `a >! n` | shift right, keeping the sign | `asr`, like `**` |
| `bnot a` | complement (a function) | |

```rust
fn main () {
    println (12 &! 10);            // 8
    println (12 |! 10);            // 14
    println (12 ^! 10);            // 6
    println (1 <! 4);              // 16
    println (256 >! 2);            // 64
    println (bnot 5 &! 7);         // 2: (bnot 5) &! 7
    println (1 + 2 &! 3);          // 3: 1 + (2 &! 3)
}
```

They are in the `bitwise` trait, so they can be defined for another type
(`impl bitwise of mytype, mytype { op &! a b { .. } .. }`). A set of small
indices is often best kept as one int, `1 <! i` being the bit `i`, and
`state ^! mask` toggling several at once.

## Recursion

A function that calls itself is automatically recursive: there is no `rec`
keyword. Functions can also be nested inside other functions.

The OCaml compiler optimizes **tail-recursive** functions (where the
recursive call is the very last thing done), so they never overflow the
stack:

```rust
// Not tail-recursive: `n * ..` still has to run after the recursive call
fn fact n {
    if n <= 0 then 1 else n * (fact (n - 1))
}

// Tail-recursive, with a nested helper and an accumulator
fn fact n {
    fn aux acc n' {
        if n' <= 0 then acc else aux (n' * acc) (n' - 1)
    }
    aux 1 n
}

// Shortest: a fold over a range (see the next chapter)
fn fact n {
    fold_left (*) 1 (1..=n)
}
```

> [!NOTE]
> These three definitions are alternatives. Keep only one in a program.

## Anonymous functions

An anonymous function (a lambda) is written with `^`, its parameters, then
`->` and its body:

```rust
fn main () {
    let double = ^x -> x * 2;
    println (double 2);                             // 4

    let l = map (^x -> (x * 3 + 1) % 4) [1; 2; 3];
    println l;                                      // [ 0; 3; 2 ]
}
```

Multiple instructions in the lambda expression ? Not a problem add `{` `}` :

```rust
let print_double = ^x -> {
    println x;
    x * 2
};
```

## η-reduction

A function that only passes its last arguments on can drop them, as in
OCaml (η-reduction): a `fn` without any parameter is its body, which must
then be a function itself.

```rust
fn mul a b { a * b }

fn triple x { mul 3 x }     // can be written
fn triple { mul 3 }         // the same function

fn main () {
    println (triple 5);     // 15
}
```

So a function is either η-reduced or takes `()`: `fn hello { println
"hello" }` is an error (its body is no function), write `fn hello () {
println "hello" }`. A value is a [constant](#constants), `let answer = 6
* 7;`, not a `fn`.

## `function`

`function` is a function that matches its one argument right away, without
naming it: `function | p1 -> e1 | p2 -> e2` is `^x -> match x { | p1 ->
e1 | p2 -> e2 }`. With η-reduction, it gives a function defined by its
cases:

```rust
type a tree { Leaf | Node of a tree, a, a tree }

fn size {
    function
    | Leaf -> 0
    | Node (l, _, r) -> size l + 1 + size r
}

fn main () {
    println (size (Node (Node (Leaf, 1, Leaf), 2, Leaf)));              // 2
    println (map (function | Some x -> x | None -> 0) [Some 1; None]);  // [ 1; 0 ]
}
```

Unlike `match`, its cases have no braces around them: they go on as far as
they can. So, as in OCaml, a `function` inside one of the cases of another
takes all the cases after it; put it in parentheses to end it:

```rust
fn classify {
    function
    | Some x -> (function | 0 -> "zero" | _ -> "other") x
    | None -> "none"
}

fn main () {
    println (map classify [Some 0; Some 5; None]);  // [ "zero"; "other"; "none" ]
}
```

## Optional arguments

As in OCaml, a parameter of a `fn` can be optional:

- `?(x=0)` gives it a default value: in the body, `x` is the value given,
  or `0`.
- `?x` alone makes it an option: in the body, `x` is `Some v` when a value
  `v` is given, `None` when not.

At the call site, an optional argument is passed by its name, `x=<value>`,
anywhere among the other arguments, or left out:

```rust
fn foo ?(x=0) y { x + y }

fn bar ?x y {
    match x {
        | Some v -> v
        | None -> y
    }
}

fn main () {
    println (foo x=1 2);        // 3
    println (foo 2 x=1);        // 3
    println (foo 2);            // 2
    println (foo x=(1 + 2) 2);  // 5

    println (bar x=1 2);        // 1
    println (bar 2);            // 2
    println (bar ?x=None 2);    // 2
}
```

A few rules:

- The value after `x=` is a single term: a name, a literal, `t[i]`...
  Wrap anything else in parentheses: `x=(1 + 2)`, `x=(f 1)`.
- `?x=` (with the `?`) passes the option itself, `Some ..` or `None`,
  instead of the value: `bar ?x=(Some 1) 2` is `bar x=1 2`.
- An optional argument can only be left out when a positional one follows
  it. In `fn only ?(x=0) { x }`, `x` must always be given, and OCaml warns
  that it "cannot be erased".

When an optional argument is left out, the `Some` branch never runs, so
it doesn't matter that nothing fixes the type of its value:

```rust
fn show_sum ?u v {
    match u {
        | Some x -> println (x + v)
        | None -> println ("Alone : " + (to_string v))
    }
}

fn main () {
    show_sum 2;         // Alone : 2
    show_sum u=1 2;     // 3
}
```

## Indexing operators

`a[i]` calls the operator `[]`, and `a[i] = v` the operator `[]=`: the
standard library defines them for arrays, strings and matrices, and you
can define them for your own types. More generally, `[]` followed by any
operator symbol is an indexing operator, used as `a[i] <symbol> v`. The
standard library has `[]+=` for arrays, and you can add your own:

```rust
op []<- a i v { let b = Array.copy a; b[i] = v; b }    // a copy, changed

fn main () {
    let a = [|1; 2; 3|];
    a[0] += 10;
    a[1] += 1 + 2;              // a[1] += (1 + 2)
    println a;                  // [| 11; 5; 3 |]

    let b = a[0] <- 0;
    println b;                  // [| 0; 5; 3 |]
    println a;                  // [| 11; 5; 3 |]: unchanged
}
```

A few rules:

- `[]` takes 2 arguments (the container, the index); `[]=`, `[]+=` and
  every other one with a symbol after `[]` take 3 (the container, the
  index, the value).
- `a[i] += v` is an expression: its value is the operator's (`()` for
  `[]=` and `[]+=`, a new array for `[]<-`), and the value `v`
  extends as far as it can, like `a[1] += 1 + 2`.
- `a[i] + v` stays an addition on `a[i]` as long as no operator `[]+` is
  declared: the indexing form is only used for operators that exist.
- Several indices work like for `[]=`: `m[i, j] += v` changes `m[i][j]`,
  and `m[0..n, j] += v` does it on each of those rows.

### Lookups that may fail: `[]?`

`a[i]?` calls the operator `[]?`. It reads like `[]`, but a type can answer
it differently: in the standard library, with an `option` instead of an
exception when there is nothing at that index. `a[i]` itself is unchanged.

```rust
#use "stdlib/hashmap.scaml"

fn main () {
    let a = [|10; 20; 30|];
    println a[1]?;                      // Some 20
    println a[3]?;                      // None: out of bounds
    println a[-1]?;                     // None

    let h = make_hashmap 4;
    h["a"] = 1;
    println h["a"]?;                    // Some 1
    println h["zz"]?;                   // None, where h["zz"] raises Not_found

    let i = 5;
    match a[i]? {
        | Some v -> println v
        | None -> println "out of range"      // out of range
    }
}
```

It is defined for arrays, strings (with an index or a range) and hash maps, and you can define it for your own
types like `[]`: `op []? a i { .. }`. A few rules:

- `[]?` takes 2 arguments, the container and the index, and returns what you
  choose, usually an `option`.
- The `?` is glued to the `]`: `f a[i]? x` is the lookup, while `f a[i] ?x=1`
  (a space) passes the [optional argument](#optional-arguments) `x`.
- The `?` form is only used once an operator `[]?` is declared, as for the
  other indexing operators.
- With several indices, each one is looked up in turn and the first to fail
  gives `None`: `m[i, j]?` is `m[i]?` then `j` in that row, so `m[-1, -1]?`
  and `m[7, 0]?` are `None`. (After a range, `m[0..n, j]?`, the earlier
  indices read normally.)

## Constants

A constant is a `let` outside any function, written like a `let` inside
one, `;` included:

```rust
let pi = 3.14159;
let max_size = 100;
let (width, height) = (800, 600);       // any pattern, as in a block
let label = "size: " + to_string max_size;

fn area r { pi * r * r }

fn main () {
    println (area 2.);                  // 12.56636
    println (width * height);           // 480000
    println label;                      // size: 100
}
```

A few rules:

- A constant's name starts with a lowercase letter, like a variable's:
  `PI` would be a constructor (as in OCaml), write `pi`.
- It is defined before what uses it, like a function.
- It is computed once, when the program starts, before `main`.

---

[← Introduction](00_introduction.md) · [Index](README.md) · [Next: Loops →](02_loops.md)

<sub>© 2026 wer · SCaml is released under the [MIT License](../LICENSE).</sub>
