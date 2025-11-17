# Unicode Operator Support

This document explains how Unicode operators from GHC's UnicodeSyntax extension are handled.

## Overview

The filter provides comprehensive support for all GHC UnicodeSyntax extension operators as documented in:
https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/unicode_syntax.html

## Supported Operators

| ASCII | Unicode | LaTeX Command | Description |
|-------|---------|---------------|-------------|
| `forall` | `∀` | `\forall` | Universal quantification |
| `::` | `∷` | `\:` | Type annotation |
| `=>` | `⇒` | `\Rightarrow` | Class constraint |
| `->` | `→` | `\to` | Function type |
| `<-` | `←` | `\gets` | Bind/generator |
| `>-` | `⤚` | `\succ` | Right arrow-tail |
| `-<` | `⤙` | `\prec` | Left arrow-tail |
| `>>-` | `⤜` | `\rr` | Right double arrow-tail |
| `-<<` | `⤛` | `\ll` | Left double arrow-tail |
| `*` | `★` | See below | Star/multiplication |
| `(|` | `⦇` | `\llparenthesis` | Parallel array open |
| `|)` | `⦈` | `\rrparenthesis` | Parallel array close |
| `[|` | `⟦` | `\llbracket` | Quasiquote open |
| `|]` | `⟧` | `\rrbracket` | Quasiquote close |
| `%1->` | `⊸` | `\multimap` | Linear function type |

**Guarantee**: ASCII and Unicode versions produce identical LaTeX output.

## The Star Operator (`*` vs `★`)

### The Problem

The `*` character has two different meanings in Haskell:
1. **Multiplication operator**: `2 * 3`, `x * y` → should render as `\times`
2. **Kind operator (legacy)**: `* -> *`, `Proxy :: *` → should render as `\star`

Unfortunately, tokenizers (both GHC's lexer and Skylighting) cannot distinguish between these contexts—they produce the same token type regardless of usage.

### Our Solution

**Decision**: `*` renders as `\times` (multiplication)

**Rationale**:
1. Modern Haskell (GHC 8.0+) uses `Type` instead of `*` for kinds
2. This filter targets new code
3. Multiplication is more common in displayed code than legacy kind syntax
4. Unicode `★` is available for explicit kind annotations

### Workaround for Legacy Code

If you have legacy code using `*` for kinds:
1. Use Unicode `★` instead (renders as `\star`)
2. Migrate to `Type` (recommended)
3. TODO: Runtime configuration option (planned feature)

### Examples

```haskell
-- Multiplication (renders correctly as \times)
factorial n = product [1..n]
area = width * height

-- Legacy kind syntax (renders as \times, may look odd)
data Proxy (a :: *) = Proxy

-- Modern kind syntax (recommended)
data Proxy (a :: Type) = Proxy

-- Explicit Unicode star (renders as \star)
data Proxy (a :: ★) = Proxy
```

## Tokenizer Differences

### Root Cause

The Haskell and Skylighting tokenizers produce different token types for the same operators:

| Operator | Haskell Tokenizer | Skylighting Tokenizer |
|----------|-------------------|----------------------|
| Most operators | `TOther` | `TOperator` or `TOther` |
| `*`, `>-`, `-<`, `>>-`, `-<<` | `TOther` | `TOperator` |
| Unicode `∀` | `TKeyword` | `TOther` |
| Unicode `λ` | `TVar` | `TOther` |

### Solution

We use the `OpOrOther` pattern synonym (PatternSynonyms extension) to handle both token types uniformly:

```haskell
pattern OpOrOther :: Text -> (MyTok, Text)
pattern OpOrOther txt <- (matchOpOrOther -> Just txt)

formatToken (OpOrOther "->") = mathop "to"
formatToken (OpOrOther "→")  = mathop "to"  -- Same LaTeX output
```

This eliminates code duplication while handling operators from both tokenizers correctly.

## Additional Fixes

### Unicode Lambda (λ)

Unicode lambda characters can appear as `"λx"` (single token). We split them:
```haskell
splitUnicodeLambda: "λx" → ["λ", "x"]
```

### Bracket Operators

Skylighting incorrectly splits GHC bracket operators. We merge them:
```haskell
fixBracketOperators:
  "(", "|" → "(|"
  "|", ")" → "|)"
  "[", "|" → "[|"
  "|", "]" → "|]"
```

## Testing

All Unicode operators have comprehensive unit tests in `test/UnicodeOperatorSpec.hs`:
- Tests verify ASCII and Unicode versions produce identical output
- Tests verify both Haskell and Skylighting tokenizers work correctly
- Tests use `error` instead of warnings (enforced failures)

## Required LaTeX Packages

The following LaTeX packages are automatically included:
- `amssymb` - Basic math symbols
- `amsmath` - Advanced math support
- `stmaryrd` - St Mary's Road symbols (for bracket operators)

## Future Work

- [ ] Add runtime configuration for `*` rendering (`\times` vs `\star`)
- [ ] Add Pandoc metadata support for per-document configuration
- [ ] Consider context-aware rendering (if feasible without full parsing)
