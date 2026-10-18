# SCaml for Neovim

Syntax highlighting and completion for SCaml (`.scaml` files).

## Install

With [lazy.nvim](https://github.com/folke/lazy.nvim), from a clone of the
repository:

```lua
{ dir = "~/path/to/scaml/editors/nvim", name = "scaml", lazy = false }
```

Or add this folder to the `runtimepath`:

```lua
vim.opt.runtimepath:prepend("~/path/to/scaml/editors/nvim")
```

## Completion

The standard library's functions are completed by the plugin's
`omnifunc` (`<C-x><C-o>` in insert mode):

- trait methods (`print`, `map`, `to_char`...) with the signature their
  trait declares (`a -> unit`), the trait and the types implementing it;
- functions (`sum`, `hashmap_of_list`, `to_matrix`...) with their
  parameters (their types are inferred, written nowhere) and the comment
  above them;
- only what the file can use: `core.scaml`, plus the files it `#use`s
  (and those they `#use`);
- OCaml's modules: `Array.` offers `make`, `init`, `map`... with their
  type and documentation, read from the `.mli` files of the compiler
  (`ocamlc -where`, or `vim.g.scaml_ocaml_stdlib`); a capitalized word
  offers the module names (`Arr` → `Array`, `ArrayLabels`).

No language server: `stdlib/` is read once, and again when a file of it
changes. It is found next to `editors/`; set `vim.g.scaml_stdlib` to use
another one.

With a completion plugin, add its `omni` source for SCaml files:

```lua
-- blink.cmp (e.g. in LazyVim)
{ "saghen/blink.cmp", opts = { sources = { per_filetype = { scaml = { inherit_defaults = true, "omni" } } } } }

-- nvim-cmp, with hrsh7th/cmp-omni
require("cmp").setup.filetype("scaml", { sources = { { name = "omni" }, { name = "buffer" } } })
```
