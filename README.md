# SCaml

**SCaml is for Simple Caml**. This is a transpiled langage in OCaml, that make functionnal programming langage easier by simplifing the syntax of such langage.

Using syntax inspiration from several langage such as Rust.

SCaml -> OCaml -> Assembly (opt) -> Binary

## Architecture

There is no custom AST. The parser builds real `Parsetree.expression` values
(OCaml's own AST, from `compiler-libs`), which lets the rest of the pipeline
reuse OCaml's own tooling end to end:

1. `lib/lexer.mll` (ocamllex) — tokenizes SCaml source.
2. `lib/parser.mly` (Menhir) — builds `Parsetree.expression` nodes directly,
   using `Ast_helper` (e.g. `a + b` becomes `Exp.apply (Exp.ident "+") [a; b]`,
   exactly like the real OCaml parser does).
3. `Typecore.type_expression` (compiler-libs) — type-checks the Parsetree
   with OCaml's real type-checker. Type errors are OCaml's own error
   messages, with real source locations.
4. `Pprintast.expression` (compiler-libs) — pretty-prints the Parsetree back
   into valid OCaml source text.
5. `ocamlfind ocamlopt` — compiles the generated `.ml` file into a real
   native executable.

`bin/main.ml` wires all of this together.

## Dependencies

**Need OCaml 5.5**

## Build & run

```bash
dune build
dune exec bin/main.exe -- examples/hello.scaml
./examples/hello.exe
```

This prints the inferred type, the generated OCaml source, then compiles and
runs it.

By default only the executable (`examples/hello.exe`) is written next to the
source. Pass `--keep` (or `-k`) to also keep the intermediate files there:
`hello.generated.ml`, `.cmi`, `.cmx` and `.o`.

## Extending the language

1. Add new keywords/operators in `lib/lexer.mll`.
2. Add the corresponding `%token` declarations and grammar rules in
   `lib/parser.mly`, building the matching `Parsetree` node via `Ast_helper`
   (see `Ast_helper.mli` in `compiler-libs` for the full API).
3. No AST module to update — the OCaml AST already has a node for anything
   the language needs (records, variants, pattern matching, modules, ...).
4. Run `dune build` — Menhir reports grammar conflicts in
   `_build/default/lib/parser.conflicts` if they occur.

Command to show OCaml's own parsing tree for reference:

```bash
ocamlfind ocamlc -dparsetree -c myfile.ml
```

## License

SCaml is released under the [MIT License](LICENSE), © 2026 wer.

It builds on OCaml's `compiler-libs` (parser, type-checker, pretty-printer),
distributed under the GNU LGPL 2.1 with the OCaml special exception on
linking, which allows distributing the SCaml compiler, and the programs it
compiles, under terms of one's choice. `reference/ast_helper.ml` is a copy of
an OCaml source file, kept for reference only: it remains under OCaml's own
license (see its header), not SCaml's.
