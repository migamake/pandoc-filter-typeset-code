# Changelog for pandoc-filter-indent

## 0.3.3.0 Nov 17 2025

### Unicode and Operator Support
  - Add comprehensive support for GHC UnicodeSyntax extension operators
    - Unicode lambda (λ) now correctly renders as `\lambda`
    - All GHC UnicodeSyntax operators now supported: ∷, ⇒, →, ←, ⤚, ⤙, ⤜, ⤛, ★, ∀, ⦇, ⦈, ⟦, ⟧, ⊸
    - ASCII and Unicode versions of operators produce identical LaTeX output
  - Fix unicode lambda (λ) character handling in both Haskell and Skylighting tokenizers
    - Add `splitUnicodeLambda` to handle tokens like "λx" → ["λ", "x"]
  - Fix lambda expressions rendering in LaTeX math mode
  - Fix backslash operators (\, \\, \/, /\, etc.) rendering
  - Add preprocessing for Skylighting tokenizer to handle split backslash operators
  - Add `fixBracketOperators` to merge GHC bracket operators: (|, |), [|, |]
  - >< as Cartesian product (\times)
  - :-> as \longmapsto
  - **Design choice**: `*` renders as `\times` (multiplication) since modern Haskell uses `Type` for kinds
    - Unicode `★` renders as `\star` for legacy kind annotations
    - Tokenizers cannot distinguish between kind and multiplication contexts
    - TODO: Make this configurable for projects with legacy kind syntax

### Tokenizer Differences and Solutions
  - **Root cause identified**: Haskell and Skylighting tokenizers produce different token types
    - Haskell tokenizer: most operators → TOther
    - Skylighting tokenizer: some operators → TOperator (e.g., *, >-, -<, >>-, -<<)
    - Unicode ∀: TKeyword (Haskell) vs TOther (Skylighting)
    - Unicode λ: TVar (Haskell) vs TOther (Skylighting)
  - **Solution**: Introduced `OpOrOther` pattern synonym using PatternSynonyms extension
    - Eliminates code duplication when handling same operator from both tokenizers
    - Cleaner syntax: `formatToken (OpOrOther "->") = mathop "to"`

### Bug Fixes
  - Fix `extraColumns` and `tableColumns` deduplication mismatch
    - Both now use text column position deduplication to avoid gaps in table column indices
    - Fixes `prop_tableColumns` test failure on inputs like "\n a"
  - Rename subscripts to subAndSuperscripts with proper superscript support

### Testing
  - Add comprehensive test suite for lambda expressions and backslash operators
  - Add unit tests for all GHC UnicodeSyntax operators (UnicodeOperatorSpec)
    - Tests verify ASCII and Unicode versions produce identical LaTeX output
    - Tests now fail on error instead of just warning (enforced with error function)
  - Add tokenizer comparison tests (TokenizerTest)
  - Refactor test suite: extract common test pattern into helper function
  - Improve test readability: reduce code duplication in property tests

### Code Quality
  - Code cleanup: remove unused LANGUAGE pragmas, add missing pragmas
  - Code cleanup: use newtype for single-field records, use concatMap and elemIndex
  - Code cleanup: remove redundant $ operators and brackets
  - Code cleanup: fix pattern matching style (otherwise -> _)
  - Code cleanup: use hPrint instead of hPutStrLn . show

### Documentation
  - Add UNICODE_OPERATORS.md explaining Unicode operator support, design decisions, and limitations
  - Document tokenizer differences and solutions
  - Document the `*` vs `★` rendering choice with rationale

## Release history

0.3.2.0 Sep 29 2021
  - stacked subscripts
  - lambda in Haskell tokenizer
  - greek letters for special variable names: eps, pi, rho
  - automatically append required LaTeX packages (broken for Beamer output)
  - Mapsto for |=>

0.3.1.0 Jan 15 2021
  - Slightly improved formatting with SkyLighting and LaTeX

0.3.0.0 Jan 15 2021
  - Support default syntaxes from skylighting.

0.2.3.0 Jan 12 2021
  - fixed minor issue that disabled inline rendering.

0.2.2.0 Jan 12 2021
  - Fixed problem with declaring too few columns in LaTeX
  - Inline code formatting
  - Removed indent marks from parenthesised operators: "(+)"
  - Removed indent marks from functions promoted to operators like "`mappend`"
  - Fixed rendering of indent mark at the start of the column

0.1.0.0 Dec 1 2020
  - Initial release supporting LaTeX and HTML output

0.2.0.0 Dec 2 2020
  - Preliminary support for Tikzmarks
  - Updated README

0.2.1.0 Dec 2 2020
  - Extended code documentation
  - Cleanups
