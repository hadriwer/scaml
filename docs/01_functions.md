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

---

[← Introduction](00_introduction.md) · [Index](README.md) · [Next: Loops →](02_loops.md)

<sub>© 2026 wer · SCaml is released under the [MIT License](../LICENSE).</sub>
