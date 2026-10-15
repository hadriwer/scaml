# Functions and operators

## Defining functions and operators

Functions and operators share the same definition syntax:

```text
fn <name> <args>* { <body> }
op <symbols> <arg1> <arg2> { <body> }
```

The only difference is how they are called. A function is **prefix** (its
name comes first), while an operator, made of symbols, is **infix** (it goes
between its two arguments). Wrapping an operator in parentheses turns it
back into a prefix function:

```rust
fn foo a b { print a; print " << "; println b; }

op << a b { print a; print " << "; println b; }

fn main {
    foo "a" 1;      // a << 1

    1 << "a";       // 1 << a
    (<<) 1 "a";     // 1 << a
}
```

A few rules:

- The entry point of a program is the `main` function.
- A function without parameters takes `unit`: `fn main { .. }` is called as
  `main ()`.
- The last expression of a block is its return value. No `return` keyword
  is needed.
- An operator takes exactly two arguments.

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

fn main {
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
fn main {
    println "Hello, World!"
}
```

And your first operator, the sum of absolute values:

```rust
op |+| x y {
    (abs x) + (abs y)
}

fn main {
    println (-1 |+| -2)     // 3
}
```

An operator's precedence comes from its first character, as in OCaml. `|+|`
starts with `|`, so it binds like a comparison, while an operator starting
with `*` binds like a multiplication.

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
fn main {
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

fn main {
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

fn main {
    show_sum 2;         // Alone : 2
    show_sum u=1 2;     // 3
}
```

---

[← Introduction](00_introduction.md) · [Index](README.md) · [Next: Loops →](02_loops.md)

<sub>© 2026 wer · SCaml is released under the [MIT License](../LICENSE).</sub>
