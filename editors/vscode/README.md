<p align="center">
  <img src="images/icon.png" alt="SCaml logo" width="128">
</p>

# SCaml for VS Code

Syntax highlighting for SCaml (`.scaml` files).

## Install (local)

Link this folder into VS Code's extensions directory, then reload VS Code:

```bash
ln -s "$(pwd)" ~/.vscode/extensions/scaml
```

Or build a `.vsix` package:

```bash
npx @vscode/vsce package
code --install-extension scaml-0.1.0.vsix
```

## File icon

`.scaml` files get the SCaml camel as their icon. VS Code shows it with
file icon themes that fall back to language icons (e.g. the default *Seti*);
themes that ship their own icon set may override it.

## Highlighted

- `//` comments, `#use` directives
- keywords: `fn`, `op`, `let`, `type`, `trait`, `impl`, `of`, `match`, `if`, `then`, `else`
- lambdas `^x y -> ...` and their parameters
- names declared by `fn`, `op`, `type`, `trait`, `impl`, `let`
- constructors (`Some`, `Node`), modules (`Array.make`), record fields (`p.name`)
- strings with escapes (`\n`, `\t`, `\"`, `\\`), chars (`'a'`), ints, floats
- builtin types (`int`, `float`, `string`, `list`, `array`, ...)
- operators (`|>`, `::`, `..`, `..=`, `->`, ...) and operator sections (`(+)`)
