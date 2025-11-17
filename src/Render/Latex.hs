{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE PatternSynonyms   #-}
-- | Render analyzed input into LaTeX table.
module Render.Latex(latexFromColSpans, latexInline, latexPackages, subAndSuperscripts) where

import           Data.Text(Text)
import qualified Data.Text as T
import           Data.Char(isAlpha, isAsciiLower, isAsciiUpper, isDigit)

import           Text.LaTeX.Base.Syntax(protectText)
import           Data.Maybe (fromMaybe)

import           Alignment ( Align(..) )
import           Render.Common(TokensWithColSpan)
import           Token(MyTok(..))
import           Util(unbrace)

-- | Protect special LaTeX characters for use in math mode.
--   Handles Haskell operators with backslashes that Skylighting doesn't tokenize properly.
protectTextMath :: Text -> Text
protectTextMath = processText
  where
    processText :: Text -> Text
    processText t
      | T.null t = ""
      -- Handle specific multi-character operators first
      | "\\\\" `T.isPrefixOf` t = "\\setminus " <> processText (T.drop 2 t)
      | "\\/" `T.isPrefixOf` t = "\\land " <> processText (T.drop 2 t)
      | "/\\" `T.isPrefixOf` t = "\\lor " <> processText (T.drop 2 t)
      -- Handle lambda: backslash followed by letter/digit
      | Just ('\\', rest) <- T.uncons t
      , Just (nextChar, _) <- T.uncons rest
      , isAlphaNum nextChar = "\\lambda " <> processText rest
      -- Handle standalone backslash followed by operator chars
      | Just ('\\', rest) <- T.uncons t = "\\backslash " <> processText rest
      -- Handle other special chars
      | otherwise =
          let (c, rest) = fromMaybe (' ', "") $ T.uncons t
          in escapeChar c <> processText rest

    escapeChar :: Char -> Text
    escapeChar '#'  = "\\#"
    escapeChar '$'  = "\\$"
    escapeChar '%'  = "\\%"
    escapeChar '&'  = "\\&"
    escapeChar '_'  = "\\_"
    escapeChar '{'  = "\\{"
    escapeChar '}'  = "\\}"
    escapeChar c    = T.singleton c

    isAlphaNum :: Char -> Bool
    isAlphaNum c = isAsciiLower c || isAsciiUpper c || isDigit c

-- | Given a number of table columns,
--   and a list of lists of colspans for each table row,
--   return raw LaTeX code.
latexFromColSpans :: Int -> [[TokensWithColSpan]] -> Text
latexFromColSpans cols =
    wrapTable cols
  . T.unlines
  . fmap ( (<> "\\\\")
         . T.intercalate " & "
         . fmap renderColSpan )

-- | Render a single colspan as LaTeX \multicolumn.
renderColSpan :: TokensWithColSpan -> Text
renderColSpan ([(TBlank, txt)], colSpan, AIndent) = -- indentation
    T.concat [ "\\multicolumn{",    T.pack $ show colSpan
                          , "}{p{", T.pack $ show $ T.length txt
                          , "ex}}{",  protectText txt
                          , "\\,}" ]
renderColSpan (toks, colSpan, alignment) =
    T.concat [ "\\multicolumn{",  T.pack $ show colSpan
                          , "}{", alignMark alignment
                          , "}{$", latexInline toks
                          , "$}" ]
  where
    alignMark ACenter = "c"
    alignMark ALeft   = "l"
    alignMark AIndent = "l"

-- | Wrap a LaTeX table content into \begin{tabular} environment.
wrapTable :: Int -> Text -> Text
wrapTable cols txt =
  mconcat [-- "\\newlength{\\tabcolsepBACKUP}\n"
           -- ,"\\setlength{\\tabcolsepBACKUP}{\\tabcolsep}"
            "\\setlength{\\tabcolsep}{1pt}\n"
          , "\\begin{tabular}{"
          , T.replicate (cols+1) "l" -- FIXME: tests for correct number of columns
          , "}\n"
          , txt, "\n\\end{tabular}"
           --,"\\setlength{\\tabcolsep}{\\tabcolsepBACKUP}"
          ]

-- Decrease column spacing: \\setlength{\\tabcolsep}{1ex}
-- TODO: braced operators
-- | Preprocesses functions converted to operator syntax and joins them into a single token.
-- FIXME: deduplicate
preformatTokens []                                                     = []
preformatTokens ((TOther, "`"):(TVar, "elem"):(TOther, "`"):rest) = (TOperator, "elem"):preformatTokens rest
preformatTokens (a                                              :rest) =  a                 :preformatTokens rest


-- | Format a list of tokens within a colspan.
--   Preprocesses then and calls `formatToken` for each.
latexInline :: [(MyTok, Text)] -> Text
latexInline  = T.concat
             . fmap formatToken
             . preformatTokens

-- | Add subscripts and superscripts to variable names.
--   "_" is subscript, and "__" is superscript.
--   Superscripts nest their remaining content recursively.
subAndSuperscripts :: Text -> Text
subAndSuperscripts ""  = " "
subAndSuperscripts "_" = "\\_"
subAndSuperscripts t   = case T.splitOn "_" t of
  [] -> ""
  (x:xs) -> x <> processSegments xs
  where
    -- Process segments after splitting by "_"
    -- Empty string indicates double underscore (superscript)
    processSegments :: [Text] -> Text
    processSegments [] = ""
    processSegments ("":rest) = case rest of
      [] -> ""  -- Trailing "__"
      _  -> let remainingText = T.intercalate "_" rest
            in "\\textsuperscript{" <> subAndSuperscripts remainingText <> "}"
    processSegments (x:xs) = "\\textsubscript{" <> x <> "}" <> processSegments xs

-- | Pattern for operator-like tokens (TOperator or TOther).
--   Both tokenizers produce different token types for the same operators:
--   - Haskell tokenizer: most operators → TOther
--   - Skylighting tokenizer: some operators → TOperator (e.g., *, >-, -<, >>-, -<<)
--   This pattern synonym eliminates duplication when handling the same operator from both tokenizers.
pattern OpOrOther :: Text -> (MyTok, Text)
pattern OpOrOther txt <- (matchOpOrOther -> Just txt)

matchOpOrOther :: (MyTok, Text) -> Maybe Text
matchOpOrOther (TOperator, txt) = Just txt
matchOpOrOther (TOther,    txt) = Just txt
matchOpOrOther _                = Nothing

-- Workaround with joinEscapedOperators til w consider spaces only.
-- | Render a simple token.
formatToken :: (MyTok, Text) -> Text
formatToken (TOperator,unbrace -> Just op) = "(" <> formatToken (TOperator, op) <> ")"
-- GHC UnicodeSyntax extension operators
-- See: https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/unicode_syntax.html
-- Note: ∀ can be TKeyword (Haskell) or TOther (Skylighting)
formatToken (TKeyword, "forall") = mathop "forall"
formatToken (TKeyword, "∀"     ) = mathop "forall"
formatToken (OpOrOther "∀") = mathop "forall"
--formatToken (TVar,     "mempty") = mathop "emptyset"
formatToken (TVar,     "bottom") = mathop "bot"
formatToken (TVar,  "undefined") = mathop "perp"
formatToken (TVar,     "top"   ) = mathop "top"
formatToken (TVar,     "not"   ) = mathop "neg"
--formatToken (TOperator,">>="   ) = mathop "mathbin{>\\!\\!\\!>\\!\\!=}" -- from lhs2TeX, Neil Mitchell's
formatToken (TOperator,">>="   ) = mathop "mathbin{\\gg\\!\\!=}" -- from lhs2TeX, Neil Mitchell's
formatToken (TOperator,"=<<"   ) = mathop "mathbin{=\\!\\!<\\!\\!\\!<}" -- from lhs2TeX, Neil Mitchell's
formatToken (TOperator,">=>"   ) = mathop "mathbin{>\\!\\!=\\!\\!\\!>}"
formatToken (TOperator,"|-"    ) = mathop "vdash"
formatToken (TOperator,"/\\"   ) = mathop "lor"
formatToken (TOperator,"\\/"   ) = mathop "land"
formatToken (TOperator,"\\|/"  ) = mathop "downarrow"
formatToken (TOperator,"\\||/" ) = mathop "Downarrow"
formatToken (TOperator,"/|\\"  ) = mathop "uparrow"
formatToken (TOperator,"/||\\" ) = mathop "Uparrow"
formatToken (TOperator,"~>"    ) = mathop "leadsto"
formatToken (TOperator,"|="    ) = mathop "models"
formatToken (TCons    ,"Natural") = "\\mathbb{N}"
formatToken (TCons    ,"Integer") = "\\mathbb{Z}"
formatToken (TOperator,"|"     ) = mathop "alt"
formatToken (TOperator,"||"    ) = mathop "parallel"
formatToken (TOperator,"|>"    ) = mathop "triangleright"
--formatToken (TOperator,">>"    ) = mathop "mathbin{>\\!\\!\\!>}" -- gg
--formatToken (TOperator,">>>"   ) = mathop "mathbin{>\\!\\!\\!>\\!\\!\\!>}" -- gg
formatToken (TOperator,">>"    ) = mathop "gg"
formatToken (TOperator,">>>"   ) = mathop "ggg"
formatToken (TOperator,"<<"    ) = mathop "ll"
formatToken (TOperator,"<<<"   ) = mathop "lll"
formatToken (TOperator,"\\\\"  ) = mathop "setminus" -- MUST come before single backslash!
formatToken (OpOrOther "\\") = mathop "lambda" -- Lambda (both tokenizers)
formatToken (OpOrOther "λ")  = mathop "lambda" -- Unicode lambda (TOther from Skylighting)
formatToken (TVar,     "λ"     ) = mathop "lambda" -- Unicode lambda (TVar from Haskell tokenizer)
formatToken (OpOrOther "-<") = mathop "prec" -- Left arrow-tail
formatToken (OpOrOther "⤙")  = mathop "prec" -- Unicode -<
formatToken (OpOrOther ">-") = mathop "succ" -- Right arrow-tail
formatToken (OpOrOther "⤚")  = mathop "succ" -- Unicode >-
formatToken (OpOrOther "<-") = mathop "gets" -- Left arrow
formatToken (OpOrOther "←")  = mathop "gets" -- Unicode <-
formatToken (TOperator,">="    ) = mathop "geq"
formatToken (TOperator,"<="    ) = mathop "leq"
formatToken (TOperator,"!="    ) = mathop "ne"
formatToken (TOperator,"<->"   ) = mathop "updownarrow"
formatToken (TOperator,"<|>"   ) = mathop "leftrightarrow"
formatToken (OpOrOther "->") = mathop "to"
formatToken (OpOrOther "→")  = mathop "to"      -- Unicode ->
formatToken (OpOrOther "=>") = mathop "Rightarrow"
formatToken (OpOrOther "⇒")  = mathop "Rightarrow" -- Unicode =>
formatToken (OpOrOther "::") = mathop ":"       -- Type annotation
formatToken (OpOrOther "∷")  = mathop ":"       -- Unicode ::
-- Note: * is rendered as \times (multiplication) since modern Haskell uses Type for kinds
-- TODO: Make this configurable for legacy code that uses * for kinds
formatToken (OpOrOther "*")  = mathop "times"   -- Multiplication (star for old kind syntax)
formatToken (OpOrOther "★")  = mathop "star"    -- Unicode kind star
formatToken (OpOrOther ">>-") = mathop "rr"     -- Right double arrow-tail
formatToken (OpOrOther "⤜")  = mathop "rr"      -- Unicode >>-
formatToken (OpOrOther "-<<") = mathop "ll"     -- Left double arrow-tail
formatToken (OpOrOther "⤛")  = mathop "ll"      -- Unicode -<<
formatToken (OpOrOther "⊸")  = mathop "multimap" -- Unicode linear arrow %1->
formatToken (OpOrOther "(|") = mathop "llparenthesis" -- Parallel array bracket (stmaryrd)
formatToken (OpOrOther "⦇")  = mathop "llparenthesis" -- Unicode (|
formatToken (OpOrOther "|)") = mathop "rrparenthesis" -- Parallel array bracket (stmaryrd)
formatToken (OpOrOther "⦈")  = mathop "rrparenthesis" -- Unicode |)
formatToken (OpOrOther "[|") = mathop "llbracket" -- Quasiquote bracket (stmaryrd)
formatToken (OpOrOther "⟦")  = mathop "llbracket" -- Unicode [|
formatToken (OpOrOther "|]") = mathop "rrbracket" -- Quasiquote bracket (stmaryrd)
formatToken (OpOrOther "⟧")  = mathop "rrbracket" -- Unicode |]
formatToken (TOperator,"==>"   ) = mathop "implies"
formatToken (TOperator,"|->"   ) = mathop "mapsto"
formatToken (TOperator,"|=>"   ) = mathop "Mapsto" -- requires stmaryrd
formatToken (TOperator,"<>"    ) = mathop "diamond"
formatToken (TOperator,"<$>"   ) = mathop "mathbin{\\ooalign{\\raise.29ex\\hbox{$\\scriptscriptstyle\\$$}\\cr\\hss$\\!\\lozenge$\\hss}}"
formatToken (TOperator,"<*>"   ) = mathop "mathbin{\\ooalign{\\raise.37ex\\hbox{$\\scriptscriptstyle{*}$}\\cr\\hss$\\!\\lozenge$\\hss}}"
formatToken (TOperator,"elem"  ) = mathop "in"
formatToken (TOperator,"~"     ) = mathop "sim"
formatToken (TOperator,"~="    ) = mathop "approx"
formatToken (TOperator,"><"    ) = mathop "times"
formatToken (TOperator,":->"   ) = mathop "longmapsto"
formatToken (TVar,     "a"     ) = mathop "alpha"
formatToken (TVar,     "b"     ) = mathop "beta"
formatToken (TVar,     "c"     ) = mathop "gamma"
formatToken (TVar,     "d"     ) = mathop "delta"
formatToken (TVar,     "eps"   ) = mathop "epsilon"
formatToken (TVar,     "k"     ) = mathop "kappa"
formatToken (TVar,     "n"     ) = mathop "nu"
formatToken (TVar,     "m"     ) = mathop "mu"
formatToken (TVar,     "sigma" ) = mathop "sigma"
formatToken (TVar,     "omega" ) = mathop "omega"
formatToken (TVar,     "pi"    ) = mathop "pi"
formatToken (TVar,     "tau"   ) = mathop "tau"
formatToken (TVar,     "rho"   ) = mathop "rho"
formatToken (TVar    , txt     ) | T.any isAlpha txt = "\\textit{" <> subAndSuperscripts txt <> "}"
formatToken (TVar,     txt     ) = "\\textit{"     <> protectText txt  <> "}"
formatToken (TNum    , kwd     ) = protectText kwd
formatToken (TKeyword, kwd     ) = "\\textbf{"     <> protectText kwd  <> "}"
formatToken (TCons,    cons    ) = "\\textsc{"     <> subAndSuperscripts cons <> "}"
--formatToken (TOperator,"\\"    ) = mathop "lambda"
formatToken (TTikz mark,_      ) = mathop $ "tikzMark{" <> mark <> "}"
--formatToken (TOther,   "`"     ) = mathop "textasciigrave"
formatToken (TOther,   "`"     ) = protectText "`"
formatToken (TOther,   "'"     ) = mathop "prime"
formatToken (TOther,   "\""     ) = protectText "\""
formatToken (TOther,   "("     ) = protectText "("
formatToken (TOther,   ")"     ) = protectText ")"
formatToken (TOther,   "]"     ) = protectText "]"
formatToken (TOther,   "["     ) = protectText "["
formatToken (TOther,   "}"     ) = protectText "}"
formatToken (TOther,   "{"     ) = protectText "{"
--formatToken (TOther,   "="     ) = "\\scalebox{1.7}{" <> mathop "=" <> "}"
formatToken (TOther,   "="     ) = "=\\joinrel="
-- formatToken (TBlank,   txt  ) = "\\textit{\\textcolor{gray}{" <> protectText txt <> "}}"
formatToken (_,  txt           ) = "\\textrm{"     <> protectTextMath txt  <> "}"

mathop :: Text -> Text
mathop code = "\\" <> code

prologue :: Text
prologue = T.concat ["\\usepackage{amssymb}"]

latexPackages :: [Text]
latexPackages  = ["amssymb", "amsmath", "stmaryrd"]
