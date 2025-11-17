# Changelog for pandoc-filter-indent

## Unreleased changes

  - Fix lambda expressions rendering in LaTeX math mode
  - Fix backslash operators (\, \\, \/, /\, etc.) rendering
  - Add preprocessing for Skylighting tokenizer to handle split backslash operators
  - Rename subscripts to subAndSuperscripts with proper superscript support
  - Add comprehensive test suite for lambda expressions and backslash operators
  - >< as Cartesian product (\times)
  - :-> as \longmapsto
  - Code cleanup: remove unused LANGUAGE pragmas, add missing pragmas
  - Code cleanup: use newtype for single-field records, use concatMap and elemIndex
  - Code cleanup: remove redundant $ operators and brackets
  - Code cleanup: fix pattern matching style (otherwise -> _)
  - Code cleanup: use hPrint instead of hPutStrLn . show
  - Refactor test suite: extract common test pattern into helper function
  - Improve test readability: reduce code duplication in property tests

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
