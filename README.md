# pandoc-filter-indent

Pandoc filter that intelligently typesets code by detecting indentation boundaries and aligning operators.

## Quick Start

Install via Stack:

```bash
stack install pandoc-filter-indent
```

Use with Pandoc:

```bash
pandoc --filter pandoc-filter-indent input.md -o output.pdf
pandoc --filter pandoc-filter-indent input.md -o output.html
```

## What it does

Transforms code blocks into well-aligned, tabular layouts that highlight code structure. Instead of plain monospaced code, you get properly aligned operators, function signatures, and nested structures.

**Input:**

````markdown
```{.haskell}
class Eq      a
   => Compare a where
  compare :: a -> a -> Ordering
  (>=)    :: a -> a -> Bool
```
````

**Output:** Rendered with operators vertically aligned in clean tables, using mathematical symbols (→, ⇒, ≥) in LaTeX/PDF output.

## Features

- **Smart alignment** - Detects and aligns operators (`::`, `=`, `->`, etc.) using GHC lexer
- **Multiple languages** - Haskell (GHC), Python, or generic indentation-based formatting
- **Flexible output** - LaTeX tables, HTML tables, or Pandoc native tables
- **Symbol beautification** - Converts operators to mathematical symbols (optional)
- **Lightweight** - Simple Pandoc filter, easy to integrate

## Usage

Mark code blocks with language attribute:

````markdown
```{.haskell}
-- your Haskell code
```
````

Or use alternate lexers:

````markdown
```{.python}
# your Python code
```
````

For generic indentation-based alignment:

````markdown
```{.haskell lexer=indent}
-- any indented code
```
````

## Options

Configure via code block attributes:

- `lexer=haskell` - GHC lexer (default)
- `lexer=python3` - Python lexer
- `lexer=indent` - Indent-only alignment
- `lexer=spaces` - Space-based alignment
- `debug=true` - Show column boundaries for debugging

Example:

````markdown
```{.haskell debug=true}
yourCode :: Here
```
````

## Full Documentation

For detailed algorithm explanation, implementation guide, and advanced usage, see [DESCRIPTION.md](DESCRIPTION.md).

**See also:** [Complete Unicode operator symbol mapping](DESCRIPTION.md#appendix-operator-symbol-replacement) for all supported operator conversions (→, ⇒, ≥, ⊥, ∅, etc.).

## Links

- **GitHub:** <https://github.com/migamake/pandoc-filter-typeset-code>
- **Hackage:** <https://hackage.haskell.org/package/pandoc-filter-indent>
- **Issues:** <https://github.com/migamake/pandoc-filter-typeset-code/issues>

## License

BSD3 - see LICENSE file
