{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE FlexibleContexts      #-}
-- | Skylighting code tokenizer
module Token.Skylighting(lookupTokenizer, tokenizer, fixBackslashOperators) where

import Control.Arrow(first)
import Text.Pandoc.JSON ()
import Text.Pandoc.Definition ()
import Data.Maybe(listToMaybe, mapMaybe)
import Data.String (IsString)
import Data.Char(isAsciiLower, isAsciiUpper, isDigit)
import Data.Text (Text)
import qualified Data.Text as T
import Prelude hiding(getLine)
import Optics.Core

import qualified Skylighting.Types     as Sky
import           Skylighting.Types           (TokenType(..), Syntax, SourceLine, Token)
import qualified Skylighting.Syntax    as Sky(defaultSyntaxMap)
import qualified Skylighting.Tokenizer as Sky(tokenize, TokenizerConfig(..))
import qualified Skylighting.Core      as Sky(lookupSyntax, syntaxByShortName)

import Token ( MyLoc(MyLoc), MyTok(..), unTikzMark, mark )

rightToMaybe :: Either a b -> Maybe b
rightToMaybe (Left  err   ) = Nothing
rightToMaybe (Right result) = Just result

-- | Looks up the tokenizer from Skylighting preset library by the short name of the language.
--   Picks the first match.
lookupTokenizer :: [Text] -> Maybe Syntax
lookupTokenizer  = listToMaybe
                 . mapMaybe (Sky.syntaxByShortName Sky.defaultSyntaxMap)

-- * Haskell tokenizer frontend
-- | Attempt to tokenize input,
--   returns `Nothing` if unsuccessful,
--   so the processor can just pass input
--   further when tokenizer fails.
tokenizer :: Syntax -- Skylighting syntax description
          -> Text -- ^ Input text of code block
          -> Maybe [(MyTok, MyLoc, Text)]
tokenizer syntax =
    fmap ( joinEscapedOperators
         . fixBackslashOperators  -- Fix Skylighting's incorrect splitting of backslash operators
         . fixBracketOperators    -- Fix Skylighting's splitting of bracket operators like (|, |), [|, |]
         . splitTokens
         . restoreLocations
         . splitUnicodeLambda     -- Split unicode lambda from following characters
         . recognizeTokens )
    . rightToMaybe
    . Sky.tokenize tokenizerOpts syntax
  where
    tokenizerOpts = Sky.TokenizerConfig Sky.defaultSyntaxMap False

-- | Split tokens that start with unicode lambda (λ) into separate tokens.
--   E.g., "λx" becomes ["λ", "x"]
--   Note: λ is not officially part of GHC's UnicodeSyntax extension,
--   but is widely used as a de facto standard for backslash in lambdas.
splitUnicodeLambda :: [[(MyTok, Text)]] -> [[(MyTok, Text)]]
splitUnicodeLambda = map (concatMap splitToken)
  where
    splitToken (tok, txt)
      | "λ" `T.isPrefixOf` txt && T.length txt > 1 =
          [(TOther, "λ"), (tok, T.drop 1 txt)]
      | otherwise = [(tok, txt)]

-- | Recognize tokens from all source lines.
recognizeTokens :: [SourceLine] -> [[(MyTok, Text)]]
recognizeTokens  = map $ map $ first skyTok

-- | Convert token type of `ghc-lib` into tokens recognized by the filter.
skyTok :: TokenType -> MyTok
skyTok FloatTok          = TNum
skyTok DecValTok         = TNum
skyTok BaseNTok          = TNum
skyTok StringTok         = TString
skyTok CharTok           = TString
skyTok FunctionTok       = TVar
skyTok AttributeTok      = TBlank
skyTok VerbatimStringTok = TString
skyTok SpecialStringTok  = TCons
skyTok ConstantTok       = TCons
skyTok KeywordTok        = TKeyword
skyTok BuiltInTok        = TKeyword
skyTok PreprocessorTok   = TBlank
skyTok CommentTok        = TBlank
skyTok DocumentationTok  = TBlank
skyTok OperatorTok       = TOperator
skyTok SpecialCharTok    = TOperator
skyTok RegionMarkerTok   = TOperator
skyTok AnnotationTok     = TBlank
skyTok ControlFlowTok    = TKeyword
skyTok VariableTok       = TVar
skyTok DataTypeTok       = TCons
skyTok other             = TOther

-- FIXME: generalize for GHC tokenizer and Skylighting
-- | Restore locations
-- TESTME: test
-- 1. Without newlines should return a list of indices up to length
-- 2. Of the same length as number of tokens
-- 3. With newlines should return line indices up to number of lines.
-- 4. Same for a list of lists of words without newlines joined as lines
restoreLocations :: [[(MyTok, Text)]] -> [(MyTok, MyLoc, Text)]
restoreLocations srcLines = concat
                          $ zipWith (`go` 1) [1..] srcLines
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

-- | Fix Skylighting's tokenization of bracket operators used in GHC extensions.
--   Skylighting splits operators like (|, |), [|, |] which should be single tokens.
--   This function merges them back together.
fixBracketOperators :: [(MyTok, MyLoc, Text)] -> [(MyTok, MyLoc, Text)]
fixBracketOperators [] = []
-- (| parallel array bracket
fixBracketOperators ((TOther, loc, "("):(TOperator, _, "|"):remaining) =
  (TOperator, loc, "(|") : fixBracketOperators remaining
-- |) parallel array bracket
fixBracketOperators ((TOperator, loc, "|"):(TOther, _, ")"):remaining) =
  (TOperator, loc, "|)") : fixBracketOperators remaining
-- [| quasiquote bracket
fixBracketOperators ((TOther, loc, "["):(TOperator, _, "|"):remaining) =
  (TOperator, loc, "[|") : fixBracketOperators remaining
-- |] quasiquote bracket
fixBracketOperators ((TOperator, loc, "|"):(TOther, _, "]"):remaining) =
  (TOperator, loc, "|]") : fixBracketOperators remaining
-- Default case
fixBracketOperators (tok:rest) =
  tok : fixBracketOperators rest

-- | Fix Skylighting's incorrect tokenization of backslash operators.
--   Skylighting incorrectly splits operators like \+, \>, \/, etc.
--   This function merges them back together to match Haskell tokenizer behavior.
fixBackslashOperators :: [(MyTok, MyLoc, Text)] -> [(MyTok, MyLoc, Text)]
fixBackslashOperators [] = []
-- Standalone backslash followed by backslash -> \\ set difference operator
fixBackslashOperators ((TOther, loc, "\\"):(TOther, _, "\\"):remaining) =
  (TOperator, loc, "\\\\") : fixBackslashOperators remaining
-- Standalone backslash followed by operator symbol -> merge as operator (\+, \>, etc.)
fixBackslashOperators ((TOther, loc, "\\"):(TOperator, _, op):remaining) =
  (TOperator, loc, "\\" <> op) : fixBackslashOperators remaining
-- Standalone backslash followed by variable -> lambda, keep separate
fixBackslashOperators (tok1@(TOther, _, "\\"):rest@((TVar, _, _):_)) =
  tok1 : fixBackslashOperators rest
-- Standalone backslash followed by alphanumeric TOther -> lambda, keep separate
fixBackslashOperators (tok1@(TOther, loc1, "\\"):tok2@(TOther, loc2, txt):rest)
  | not (T.null txt) && isAlphaStart txt =
  tok1 : fixBackslashOperators (tok2:rest)
-- Token ending with backslash followed by / -> merge as \/ operator
fixBackslashOperators ((TOther, loc, txt):(TOperator, _, "/"):remaining)
  | "\\" `T.isSuffixOf` txt =
  (TOperator, loc, T.init txt <> "\\/") : fixBackslashOperators remaining
-- Operator / followed by token starting with backslash -> merge as /\ operator
fixBackslashOperators ((TOperator, loc, "/"):(TOther, _, txt):remaining)
  | "\\" `T.isPrefixOf` txt =
  (TOperator, loc, "/\\") : fixBackslashOperators ((TOther, loc, T.tail txt):remaining)
-- Token starting with backslash followed by dot (lambda notation like \f.)
fixBackslashOperators (tok1@(TOther, _, txt):tok2@(TOperator, _, "."):rest)
  | "\\" `T.isPrefixOf` txt && T.length txt > 1 && isAlphaRest (T.tail txt) =
  tok1 : tok2 : fixBackslashOperators rest
-- Default case - keep token as is
fixBackslashOperators (tok:rest) =
  tok : fixBackslashOperators rest

-- Helper functions for fixBackslashOperators
isAlphaStart :: Text -> Bool
isAlphaStart txt = case T.uncons txt of
  Just (c, _) -> isAsciiLower c || isAsciiUpper c
  Nothing -> False

isAlphaRest :: Text -> Bool
isAlphaRest = T.all (\c -> isAsciiLower c || isAsciiUpper c || isDigit c)

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

{-

-- | Recognize token using both token type from `ghc-lib`,
--   and text content.
--   Only TikZ marks are recognized by looking up text content.
recognizeToken :: [Token] -> (MyTok, Text)
recognizeToken (CommentTok, tokText@(unTikzMark -> Just mark)) =
  (TTikz mark,           tokText)
recognizeToken (tokType, tokText) =
  (skyTok       tokType, tokText)
 -}

