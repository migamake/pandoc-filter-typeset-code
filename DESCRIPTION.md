---
title: "Code typesetting made simple"
subtitle: "Pandoc filter"
description: |
  To build this document use:

  > pandoc README.md  -o README.pdf --filter=pandoc-filter-indent

date: |
  `\today`{=latex}
abstract: |
  Program code has become a prime medium for communicating important algorithmic and mathematical ideas, as
  indicated by unwavering popularity of functional pearls and multitude blog posts using literate programming style
  to illustrate key ideas.  However the literate programming systems are either appallingly complex or provide
  only limited functionality to emphasise code structure.  We propose extremely simple code typesetting tool that
  is also a Pandoc filter, and can thus be used to improve comprehensibility of the code. It is also simple enough
  to be provided as a literate program within this submission. While it is processing Haskell code, we show how it
  can be easily adapted to typeset Python, Java or C/C++.
author: Michał J. Gajda

#  - name: Michał J. Gajda
#    email: mjgajda@migamake.com
#    orcid:       "0000-0001-7820-3906"
#    affiliation: 1
affiliation:
  institution: "Migamake Pte Ltd"
  email:        mjgajda@migamake.com
  url:         "https://migamake.com"
review: true
numbersections: true
header-includes:
  - |-2
    \usepackage{graphicx}
    \DeclareUnicodeCharacter{03B1}{\ensuremath{\alpha{}}}

prologue:
  - |-2
    \usepackage{graphicx}
    \DeclareUnicodeCharacter{03B1}{\ensuremath{\alpha{}}}
    \renewcommand{\longeq}{\scalebox{1.7}[1]{=}}

---

# Introduction

Program code has become a prime medium for communicating important algorithmic and mathematical ideas, as
indicated by unwavering popularity of functional pearls and multitude blog posts using literate programming style
to illustrate key ideas.  However the literate code typesetting systems like lhs2TEX[@lhs2tex]
are either appallingly complex, language-specific, or provide only limited functionality to emphasise code structure.

We propose a simple approach that can be used not just for code in languages with syntax defined by indentation-defined
(like Haskell or Python), but also for code that has been formatted by indentation (like many Java and C/C++ projects mandate).

# Algorithm

We use `pandoc` to process only code fragments in otherwise unprocessed literate program
or article with code excerpts.

We detect layout boundaries from tokens:

(1) we note an column indent for each line^[This step is implemented using GHC API.],
then

(2) mark a start of each operator (like `::`, `=`, or `>>=`) by column.
Our second step is that of transposing a list of `Line = [(Column, Token)]`
into per-column list of indentations `[(Line, Token)]`.

(3) If any token is present after more than a single space, we also mark its beginning
as indentation boundary.

(4) After sorting columns by line number, we mark these columns that have a consistent
presence along consecutive lines as indentation anchors.

(5) Additionally we mark the leftmost indent as a same indentation barrier
as long as it follows the nesting order.

(6) For postprocessing, we escape the text according to target text processing
engine syntax, translate common operators to their ligatures (see appendix),
and output the text with layout boundaries.

We also plan to implement support for pointing to code fragments
with TikZ target marks[@tikz] at this stage^[With syntax of `{->p1-}` standing for `\tikzmark{p1}`].

We also give user an option to align code fragments without regards for operators,
in case the source programming language is not yet supported.

## Integration

This solution is provided as a `pandoc`[@pandoc] filter so that it is integrated
into standard Markdown-based processing engines with its numerous plugins
that allow inclusion of GraphViz graphs, tables from `.csv` content
or citation referencing.

# Examples

## Layout

The example input is here:
```{.haskell}
class Eq      a
   => Compare a where
  compare :: a -> a -> Ordering
  (>=)    :: a -> a -> Bool
```

After splitting input sections into separated code blocks
and further by `\n\n` markers, we get the following layout boundaries
detected.

```
cl.ass |Eq  .   . |a.
  . => |Comp.are. |a. wh.er.e
  |comp.are |:: |a. |-> |a |-> |Ordering
  |(>=).    |:: |a. |-> |a |-> |Bool
```
Here `|` marks an indentation boundary in a given line and column, whereas `.` is filler (no marker) to keep the columns indented.

First you sort by column (`Data.List.sortBy`), then within each column (`Data.List.groupBy`)
you compare token type that starts there.
As a result we see the following list of column boundaries^[Counting from column 1 and line 1.]:
```{.haskell}
[(3 -- column
 ,[3,4]) -- lines where it applies
 (7, -- column
 ,[1,2]) -- lines where it applies
,(11
 ,[3,4])
,(14
 ,[3,4])
,(15
 ,[1,2])
,(16,
  [3,4])
,(18,
  [3,4])
,(21,
  [3,4])
]
```

Another example input follows, to illustrate that we only take account
of tokens that start after at least one space:

```haskell
(\x -> x)
```
The following detection would be **wrong**:
```
|(|\|x |-|> |x)
```
Instead we detect layout boundaries as follows:
```
(\x |-> |x)
```

## Generating \LaTeX{} or HTML output

When generating \LaTeX{} or HTML output, we simply
assign a list of columns to each span of code tex in line.
This can be implemented using multicolumn marking in both output languages.
Consider the above example with columns numbered at starting character:

```
cl.ass |Eq  .   . |a.
  . => |Comp.are. |a. wh.er.e
  |comp.are |:: |a. |-> |a |-> |Ordering
  |(>=).    |:: |a. |-> |a |-> |Bool
  1    2    3   4 5 6   7  8   9
```
Alignment assignment is here:
```
cl.ass >Eq  .   . |a.
  . => >Comp.are. |a. wh.er.e
  |comp.are |:: |a. |-> |a |-> ^Ordering
  |(>=).    |:: |a. |-> |a |-> ^Bool
```
Note the use of `>` instead of `|` at the boundary of right aligned block.
We also use `^` at the right boundary of center-aligned.

That means that we produce code like this for the first line:
```latex
\multicolumn{2}{r}{class}
\multicolumn{2}{l}{Eq}
\multicolumn{6}{l}{a}
```
General syntax of
`\multicolumn`^[See [Overleaf tutorial](https://www.overleaf.com/learn/latex/tables#Combining_rows_and_columns)
if you do not know how \LaTeX{} tables work.] has three arguments, each enclosed with braces (`{}`):
1. Number of columns in the cell, for example `{2}` or `{6}`.
2. Alignment of text in the cell:
    - `{l}` for left,
    - `{r}` right,
    - `{c}` for centered.
3. The text in the cell. For example `{class}`

First two columns end at `>`. So `\multicolumn` has parameter `{2}` to indicate that the cell spans two columns (just like [`colspan="2"` in HTML](https://www.w3schools.com/tags/att_td_colspan.asp).)
`Eq ` spans another two columns (3rd and 4th column) so again it is parameter `{2}`.

You basically compute column span this way:
* Start a new column with `colspan=1` for other column markers (`<` and `|`)
* Add `+1` to the current column span for every `.`.

The third column would have code like this:
```latex
\multicolumns{1}{l}{ }
\multicolumn{2}{l}{compare}
\multicolumn{1}{c}{::}
\multicolumn{2}{l}{a}
\multicolumn{1}{c}{->}
\multicolumn{1}{l}{a}
\multicolumn{1}{c}{->}
\multicolumn{1}{l}{Ordering}
```
That is the `class` takes columns 0 and 1,
`Eq` takes columns 2 and 3, and `a` goes from column 4 til the end in column 10.

This can be easily converted to HTML table:
```
<td colspan="2" style="text-align:right">class</td>
<td colspan="2" style="text-align:left" >Eq</td>
<td colspan="2" style="text-align:left" >a</td>
```
## Pandoc filter interface

Main executable is a `pandoc` filter.
You get `pandoc` input stream, and replace
[`CodeBlock` blocks](https://hackage.haskell.org/package/pandoc-types-1.20/docs/Text-Pandoc-Definition.html#t:Block)
there with `Raw "latex"` \LaTeX{} blocks.  It is these block elements of ADT that should contain the \LaTeX{} code
Pandoc will build the document for you, and do it better than you would.
Below is a modified [example from `pandoc` documentation](https://texblog.org/2012/12/21/multi-column-and-multi-row-cells-in-latex-tables/)
for making a `pandoc` filter executable:

```{.haskell}
module Main where

import Text.Pandoc.JSON

main :: IO ()
main = toJSONFilter blockFormatter

blockFormatter :: Block -> Block
blockFormatter (CodeBlock attrs content) | isHaskell attrs =
  haskellCodeFormatter :: Text -> Text
  Raw "latex" $ haskellCodeFormatter content
blockFormatter  x = x
```

In the executable above, you write `haskellCodeFormatter` function
that takes Haskell code, and returns \LaTeX{} code _fragment_ like this:

```{.latex}
\begin{array}
\multicol{2}{..}
\end{array}
```

## Deciding between output formats

We need to check Pandoc meta before issuing `walk`
in order to check what is the output format.
In case it is \LaTeX{} or PDF, then we produce \LaTeX{} raw code fragments.
In case output format is any other, then we produce HTML table.

## Passing options to the filter

There are three ways that `pandoc` filter should accept the options:

1. On the command line `--debug`.
2. As `pandoc` metadata elements
   ([`YAML`](https://en.wikipedia.org/wiki/YAML)
    [at the start of the document](https://pandoc.org/MANUAL.html#extension-yaml_metadata_block)):
    ` pandoc-filter-indent: debug=true`
3. As attributes of the code block: ` ```{.haskell debug=true}`

All options should be available through all three option channels.
See tutorial on `optparse-applicative` to define CLI options.
To allow `YAML` option definition, add `FromJSON` instance.
For concatenating options from different sources use
this [tutorial on option merging](https://chrispenner.ca/posts/hkd-options).

There are following options that will be processed:

* `lexer` -- select a language lexer:
  - `haskell` -- default lexer, GHC
  - `indent`  -- indent only lexer: table column starts when whitespace characters at beginning of the line ends
  - `spaces`  -- space-only lexer: table column starts at the end of block of two or more whitespace characters
  - `python3` -- Python lexer (see below in @sec:python-lexer)
* `debug` -- add option that shows table columns (for debugging layout)
* `underbar` -- do not escape `_`, but instead use \LaTeX{}/HTML subscript til the end of the token:
   `method_agile` becomes `method_{agile}` in \LaTeX{}, `x_i_j` becomes `x_{i,j}` in \LaTeX{}
* table alignment (\LaTeX{} output only):
  - `array` -- use `array` environment, the default
  - `polytable` -- use `polytable` environment
* code alignment:
  - center environment -- default for `array`
  - `left` justify entire code environment -- for convenience when mentioning code inline.
* output mode -- is selected by the `pandoc` metadata only
  - `latex` -- `Raw "latex"` for `tex`, `pdf`, or `beamer` output
  - `html` -- `Raw "html"` for `html`, `slidy`, `slideous`, `s5`, or `revealjs` outputs
  - `table` -- for all other outputs we produce a `Table` block

## Appendix: operator symbol replacement

Code should be given a table of possible token replacements
depending on type of the symbol.

1. We implement greek unicode replacements for single-character type variables:
```
(TypeVar "a") -> α
```


2. We also replace the common operators with \LaTeX{} symbols commonly used for this purpose:

| Input token      | \LaTeX{} code | Rendering       |
|:-----------------|:--------------|:---------------:|
| `Operator "="`   | `\scalebox{1.7}[1]{=}`     | ＝ |
| `Operator "<>"`  | `\diamond`    | $\diamond{}$    |
| `Operator ">="`  | `\geq`        | $\geq{}$        |
| `Operator "<="`  | `\geq`        | $\leq{}$        |
| `Operator "/="`  | `\ne`         | $\ne{}$         |
| `Operator "==>"` | `\Rightarrow` | $\Rightarrow{}$ |
| `Operator "\/"`  | `\ne`         | $\bigvee{}$     |
| `Operator "/\"`  | `\ne`         | $\bigwedge{}$   |
| `Operator "."`   | `\cdot`       | $\cdot$         |
| `Operator ".."`  | `\cdots`       | $\cdots$         |
| `Operator "elem"`| `\in`         | $\in$            |
| `Operator ">>"`  | `\gg`         | $\gg$            |
| `Operator "<<"`  | `\ll`         | $\ll$            |
| `Operator "~="`  | `\approx`     | $\approx$          |
| `Operator "~"`   | `\sim`             |   $\sim$          |
| `Operator "<->"` | `\leftrightarrow` | $\leftrightarrow{}$         |
| `Operator "<|>"` | `\updownarrow`    | $\updownarrow{}$            |
| `Operator ">>>"` | `\ggg`            | $\ggg{}$         |
| `Operator "<<<"` | `\lll`            | $\lll{}$         |
| `Operator "||"`  | `\parallel`       | $\parallel{}$         |
| `Operator ">>="` | `\gg\joinrel=`             | $\gg\joinrel=$         |
| `Operator "|>"`  | `\triangleright`  | $\triangleright{}$ |
| `Operator "-<"`  | `\prec`           | $\prec{}$       |
| `Operator "<-"`  | `\gets`           | $\gets{}$       |
| `Operator "|"`   | `\vert{}`         | $\vert{}$       |
| `Operator "\\"`  | `\setminus`       | $\setminus{}$       |
| `Var "bottom"`   | `\bot`           | $\bot$            |
| `Var "top"`      | `\top`              | $\top$            |
| `Var "not"`      | `\neg`              | $\neg$            |
| `Var "mempty"`   | `\emptyset{}`     | $\emptyset{}$   |
| `Var "forall"`   | `\forall{}`       | $\forall{}$   |

3. For HTML we only replace these with either HTML entities that indicate these symbols:

```
  (Operator ">") -> &gt;
```

*Unreplaced* content should be escaped.

In order to allow easy implementation we will need alternate debugging
executable `lexer` that will just print the output of the lexer
from the code on the input:

```
[Operator "=", Var "mempty", ...]
```

For finding the right symbol replacements use:

1. [Guide to lhs2TeX](https://hackage.haskell.org/package/lhs2tex-1.18/src/doc/Guide2.pdf) is a good reference on how to format Haskell code symbols.
   See code examples laid out in this document. I expect you to find the mapping for the most commonly used operators:
   - lambda sign token: `\` shown as $\lambda$,
   - equals sign `=`,
   - function type `->`,
   - type sign `::`,
   - and operators in standard type classes:
     * `Control.Monad`: `>>=`, `>>`, `>=>` 
     * `Control.Alternative`: `<|>`
     * `Control.Functor`: `<$>`
     * `Control.Applicative`: `<*>`, `<$>`, `<*`, and `*>`
     * `Data.Semigroup.<>` shown as $\diamond$
     * `Data.Ord`: `/=`, `>=`, `<=`, `>`, `<`
     * `Num`: `+`, `-`, `*`, `/`,
     * `System.FilePath`: `</>`, `<.>`
     * `Test.QuickCheck`: `==>` shown as $\Rightarrow$
     * `Control.Arrow`: `>>>`, `***`, `&&&`, `<+>`, `^<<`, `<<^`, `>>^`, `^>>`
     * `Control.Lens`: `^.`, `^.=`.
     * arrow notation: `-<`, `>-`.
   - convert single-letter type variables (and only type variables) to greek letters:
     * `a` to `\alpha` shown as $\alpha$
     * `b` to `\beta` shown as $\beta$
     * etc.

2. [The Comprehensive LATEX Symbol List](http://tug.ctan.org/info/symbols/comprehensive/symbols-a4.pdf) is a good reference of \LaTeX{} symbol names.
   See section _3. Mathematical symbols_.

## Formatting different token types

Since we want to use `\begin{array}` environment in \LaTeX{},
we should encompass the token characters with different \LaTeX{} operators.

* variables with `\textrm{var}`
* type variables with `\mathit{tyvar}`
* others with `\textrm{}` as well
* Haskell keywords with `\textit`
This mapping should be easily changed in a single place in code.

_Please let me know if there is a question about any other token types!_

## Formatting inline code

To format inline code we lookup `inline-code` in YAML metadata,
and append it to attributes of each inline code fragment.
By default it is `haskell`.

## Safe escaping

In order to safely escape strings, we keep token type
next to original token text over entire pipeline
until rendering of code fragment as raw \LaTeX{} or HTML:
```{.haskell}
type Token... = (MyTok, Text, ...)
```

### Pandoc filter connection

Using [`Text.Pandoc.Walk`](https://hackage.haskell.org/package/pandoc-types-1.20/docs/Text-Pandoc-Walk.html)
interface we can easily implement the filter.

Filter main is a function like:
```{.haskell}
import Text.Pandoc.JSON
import Text.Pandoc.Walk

main :: IO ()
main = toJSONFilter (ourPandocWalk :: Walkable [a] Pandoc => ToJSONFilter (a -> IO [a])
```

Then we make `ourPandocWalk` to be a function that:

1. Matches [`Meta`](https://hackage.haskell.org/package/pandoc-types-1.20/docs/Text-Pandoc-Definition.html#t:Meta)
to check if we are targetting \LaTeX{} or HTML.
2. Finds [`CodeBlock`](https://hackage.haskell.org/package/pandoc-types-1.20/docs/Text-Pandoc-Definition.html#t:Block) and leaves everything else as-is.
3. Generates `\table` as [`RawBlock`](https://hackage.haskell.org/package/pandoc-types-1.20/docs/Text-Pandoc-Definition.html#t:Block)
   of \LaTeX{} output.

Options **shall** be parsed **per `CodeBlock`**.
First parameter to each `CodeBlock` are attributes ([`Attr`](https://hackage.haskell.org/package/pandoc-types-1.20/docs/Text-Pandoc-Definition.html#t:Attr)).
These attributes should be parsed as options

#### Option attributes

Per-`CodeBlock` attributes to be handled:
* `lexer=indent` or `lexer=haskell`
* ignore `CodeBlock` that does not have attribute:
  - `lexer=`
  - or `.haskell`

Global `Meta` attributes to be handled:
* output format:
  - \LaTeX{} or PDF -- produce \LaTeX{} `RawBlock`
  - HTML -- produce HTML `RawBlock`
  - or all others -- produce `Table`

### TikZ marks

TikZ marks are useful for pointing to fragments of the generated code.

You should just look for comments with syntax:
`{->markName-}` and convert them to a raw
\LaTeX{} string `\tikzmark{markName}`.

For HTML, it generates `<span id="markName" />` which you
can then draw to with [a convenient JavaScript](https://stackoverflow.com/questions/554167/drawing-arrows-on-an-html-page-to-visualize-semantic-links-between-textual-spans#623770).

### Alternate lexers

Please note that using alternate lexers
disables token replacement!
This is important, since the token replacement
to \LaTeX{} special symbols is language-specific.

#### Indent only

Indent only is simplest to implement,
just start a column after initial whitespace in each line ends.

#### Space only

There should be an option to use alternate lexer.
It is driven solely by indentation.

1. Cell break is delimited by a starting indent.
2. Or column of **consecutive** spaces that occur in more than one line,
   **and** at least one line has at least **one more** space before it.

Example input:
```{.python}
# n is size of heap
def heapify(arr, n, i):
    largest = i  # Initialize largest as root
    l = 2 * i + 1     # left = 2*i + 1
    r = 2 * i + 2     # right = 2*i + 2
```

Example column division:
```
# n is size of heap
def |heapify(arr, n, i).:
    |largest = i  # Ini.tialize largest as root
    |l = 2 * i + 1     |# left = 2*i + 1
    |r = 2 * i + 2     |# right = 2*i + 2
```

For highlighting we will later connect [`skylighting`](https://hackage.haskell.org/package/skylighting)
like Pandoc does natively.

#### Python3 lexer {#sec:python-lexer}

Very easy to support, just add package [`language-python`](https://hackage.haskell.org/package/language-python-0.5.6/docs/Language-Python-Version3-Lexer.html) to dependencies.
The token type is different, but we only ever compare it by equality.  

# Used tools

## Finding Haskell tokens

We find Haskell tokens with GHC-lib that uses GHC parser itself.
For simplicity we use [`ghc-syntax-highlighter`](https://hackage.haskell.org/package/ghc-syntax-highlighter-0.0.6.0/docs/GHC-SyntaxHighlighter.html)
We readd locations after the fact, since it gives you pure tokens with text and spaces,
or pure locations (without text).

```
tokenizeHaskell :: Text -> Maybe [(Token, Text)]
```

## \LaTeX{} output

For escaping text in TeX we use [HaTeX](http://hackage.haskell.org/package/HaTeX-3.5/docs/Text-LaTeX-Base-Render.html#t:Render)[@hatex]

We render tables inline tables with `multicolumn` (not `polytable` like lhs2TeX[@lhs2tex] does.)

We do not use [HaTeX table support](https://hackage.haskell.org/package/HaTeX-3.22.2.0/docs/Text-LaTeX-Packages-Multirow.html) yet.

## Debugging

Pandoc can automatically detect output format.
In order to get \LaTeX{} and HTML output just run:

```sh
pandoc input.md --filter=pandoc-filter-indent -o output.tex
pandoc input.md --filter=pandoc-filter-indent -o output.pdf
```

For debugging indentation use `text` output format:
```
pandoc input.md --filter=pandoc-filter-indent -o output.txt
```
In text mode, it renders indent boundaries as `|` and `^`.

Note that the filter *does not* touch the text outside code blocks.
It can add necessary \LaTeX{} headers or HTML styles in meta `headers-include`.
