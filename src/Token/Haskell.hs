{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
-- | Haskell code tokenizer
module Token.Haskell(tokenizer) where

import Text.Pandoc.JSON ()
import Text.Pandoc.Definition ()
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import Prelude hiding(getLine)
import Optics.Core

import GHC.SyntaxHighlighter
    ( tokenizeHaskell
    , Loc(..)
    , Token(..) )

import Token ( MyLoc(MyLoc), MyTok(..), unTikzMark, mark )

-- * Haskell tokenizer frontend
-- | Attempt to tokenize input,
--   returns `Nothing` if unsuccessful,
--   so the processor can just pass input
--   further when tokenizer fails.
tokenizer :: Text -- ^ Input text of code block
          -> Maybe [(MyTok, MyLoc, Text)]
tokenizer  = fmap ( joinEscapedOperators
                  . splitTokens
                  . restoreLocations
                  . splitUnicodeLambda
                  . fmap recognizeToken)
           . tokenizeHaskell

-- | Split tokens that start with unicode lambda (λ) into separate tokens.
--   E.g., "λx" becomes ["λ", "x"]
--   Note: λ is not officially part of GHC's UnicodeSyntax extension,
--   but is widely used as a de facto standard for backslash in lambdas.
splitUnicodeLambda :: [(MyTok, Text)] -> [(MyTok, Text)]
splitUnicodeLambda [] = []
splitUnicodeLambda ((tok, txt):rest)
  | "λ" `T.isPrefixOf` txt && T.length txt > 1 =
      (TOther, "λ") : splitUnicodeLambda ((tok, T.drop 1 txt):rest)
  | otherwise = (tok, txt) : splitUnicodeLambda rest

-- | Recognize token using both token type from `ghc-lib`,
--   and text content.
--   Only TikZ marks are recognized by looking up text content.
recognizeToken :: (Token, Text) -> (MyTok, Text)
recognizeToken (SymbolTok,  "\\") = -- special treatment for lambda
  (TOther,               "λ"    )
recognizeToken (SymbolTok,  "λ") = -- Unicode lambda (de facto standard)
  (TOther,               "λ"    )
recognizeToken (CommentTok, tokText@(unTikzMark -> Just mark)) =
  (TTikz mark,           tokText)
recognizeToken (tokType, tokText) =
  (haskellTok   tokType, tokText)

-- | Convert token type of `ghc-lib` into tokens recognized by the filter.
haskellTok :: Token -> MyTok
haskellTok SpaceTok       = TBlank
haskellTok CommentTok     = TBlank
haskellTok PragmaTok      = TBlank
haskellTok KeywordTok     = TKeyword
haskellTok ConstructorTok = TCons
haskellTok VariableTok    = TVar
haskellTok OperatorTok    = TOperator
haskellTok RationalTok    = TNum
haskellTok IntegerTok     = TNum
haskellTok StringTok      = TString
haskellTok CharTok        = TString
haskellTok t              = TOther

-- | Extract line number from `ghc-lib` slice location.
locLine :: Loc -> Int
locLine (Loc startLineNo startColNo _ _) = startLineNo
-- | Extract column number from `ghc-lib` slice location.
locCol :: Loc -> Int
locCol  (Loc startLineNo startColNo _ _) = startColNo

-- | Restore locations
-- TESTME: test
-- 1. Without newlines should return a list of indices up to length
-- 2. Of the same length as number of tokens
-- 3. With newlines should return line indices up to number of lines.
-- 4. Same for a list of lists of words without newlines joined as lines
restoreLocations :: [(MyTok, Text)] -> [(MyTok, MyLoc, Text)]
restoreLocations = go 1 1
  where
    go line col []              = []
    go line col ((tok, txt):ls) =
        (tok, MyLoc line col (isMark tok), txt):go newLine newCol ls
      where
        isMark TBlank = False
        isMark _      = True
        newLine  = line + lineIncr
        lineIncr = T.length $ T.filter (=='\n') txt
        newCol  | lineIncr == 0 = col + T.length txt
                | otherwise     = (+1)
                                $ T.length
                                $ fst
                                $ T.break (=='\n')
                                $ T.reverse txt

-- * Likely common with other tokenizers
-- | Split tokens into one blank per line.
-- TESTME: assures that no token has '\n' before the end of text.
splitTokens :: [(MyTok, MyLoc, Text)] -> [(MyTok, MyLoc, Text)]
splitTokens = mconcat
            . fmap splitter
  where
    splitter :: (MyTok, MyLoc, Text) -> [(MyTok, MyLoc, Text)]
    splitter (TBlank, loc@(MyLoc line _ _), txt) | T.filter (=='\n') txt /= "" =
        withLocs withNewLines
      where
        split, withNewLines :: [Text]
        split = T.lines txt
        withNewLines = fmap (<>"\n") (init split)
                    <> [last split]
        withLocs :: [Text] -> [(MyTok, MyLoc, Text)]
        withLocs (l:ls) = (TBlank, set mark True loc, l)
                        : zipWith mkEntry [line+1..] ls
        mkEntry :: Int -> Text -> (MyTok, MyLoc, Text)
        mkEntry i t = (TBlank, MyLoc i 1 True, t)
    splitter other@(_, loc@(MyLoc line 1 x), txt) = [set (_2 % mark) True other]
    splitter other                                = [other]


unmark :: Field2 a a MyLoc MyLoc => a -> a
unmark = set (_2 % mark) False

-- FIXME: use no-indent-mark instead.
joinEscapedOperators :: (Eq c, IsString c, Semigroup c) => [(MyTok, MyLoc, c)] -> [(MyTok, MyLoc, c)]
joinEscapedOperators (a@(_, _, "("):b@(_, _, _):c@(_, _, ")"):rest) =
   a:unmark b:unmark c:joinEscapedOperators rest
joinEscapedOperators (a@(_,    loc, "`"):b@(_, _, _):c@(_, _, "`"):rest) =
   a:unmark b:unmark c:joinEscapedOperators rest
joinEscapedOperators (a@(_, _, "("):b@(TOperator, _, _):rest) =
   a:unmark b:joinEscapedOperators rest
joinEscapedOperators (a@(_, _, _):b@(_, _, ")"):rest) =
   a:unmark b:joinEscapedOperators rest
joinEscapedOperators (tok:rest) = tok:joinEscapedOperators rest
joinEscapedOperators []         = []

