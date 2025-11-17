{-# LANGUAGE OverloadedStrings #-}
-- | Unit tests for GHC UnicodeSyntax extension operators
-- | See: https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/unicode_syntax.html
module UnicodeOperatorSpec (runAllTests) where

import qualified Data.Text as T
import Data.Text (Text)
import Data.Maybe (fromMaybe)

import qualified Token.Haskell as Haskell
import qualified Token.Skylighting as Sky
import Render.Latex (latexInline)

-- | Test that ASCII and Unicode versions produce identical LaTeX output
testOperatorPair :: (String, Text, Text, Text) -> IO ()
testOperatorPair (desc, asciiOp, unicodeOp, expectedLatex) = do
  putStrLn $ "  Testing: " ++ desc

  let haskellAscii = renderHaskell asciiOp
      haskellUnicode = renderHaskell unicodeOp
      skyAscii = renderSkylighting asciiOp
      skyUnicode = renderSkylighting unicodeOp

  -- Check ASCII versions produce expected LaTeX
  checkResult "Haskell (ASCII)" haskellAscii expectedLatex
  checkResult "Skylighting (ASCII)" skyAscii expectedLatex

  -- Check Unicode versions produce the same LaTeX as ASCII
  checkResult "Haskell (Unicode)" haskellUnicode expectedLatex
  checkResult "Skylighting (Unicode)" skyUnicode expectedLatex

  -- Verify ASCII and Unicode produce identical output
  if haskellAscii == haskellUnicode
    then putStrLn "    ✓ Haskell: ASCII and Unicode match"
    else error $ "    ✗ Haskell: ASCII (" ++ T.unpack haskellAscii ++
                 ") != Unicode (" ++ T.unpack haskellUnicode ++ ")"

  if skyAscii == skyUnicode
    then putStrLn "    ✓ Skylighting: ASCII and Unicode match"
    else error $ "    ✗ Skylighting: ASCII (" ++ T.unpack skyAscii ++
                 ") != Unicode (" ++ T.unpack skyUnicode ++ ")"

  putStrLn ""

checkResult :: String -> Text -> Text -> IO ()
checkResult label actual expected
  | expected `T.isInfixOf` actual =
      putStrLn $ "    ✓ " ++ label ++ ": contains " ++ T.unpack expected
  | otherwise =
      error $ "    ✗ " ++ label ++ ": expected " ++ T.unpack expected ++
              " but got " ++ T.unpack actual

renderHaskell :: Text -> Text
renderHaskell code = fromMaybe "" $ do
  tokens <- Haskell.tokenizer code
  return $ latexInline $ map (\(t,_,txt) -> (t, txt)) tokens

renderSkylighting :: Text -> Text
renderSkylighting code = fromMaybe "" $ do
  syntax <- Sky.lookupTokenizer ["haskell"]
  tokens <- Sky.tokenizer syntax code
  return $ latexInline $ map (\(t,_,txt) -> (t, txt)) tokens

-- | Test cases: (description, ASCII operator, Unicode operator, expected LaTeX command)
unicodeOperatorTests :: [(String, Text, Text, Text)]
unicodeOperatorTests =
  [ ("forall", "forall", "∀", "\\forall")
  , ("type annotation", "::", "∷", "\\:")
  , ("class constraint", "=>", "⇒", "\\Rightarrow")
  , ("function type", "->", "→", "\\to")
  , ("bind/generator", "<-", "←", "\\gets")
  , ("right arrow-tail", ">-", "⤚", "\\succ")
  , ("left arrow-tail", "-<", "⤙", "\\prec")
  , ("right double arrow-tail", ">>-", "⤜", "\\rr")
  , ("left double arrow-tail", "-<<", "⤛", "\\ll")
  , ("unicode kind star", "★", "★", "\\star")  -- Unicode only (ASCII * is multiplication)
  , ("linear arrow", "⊸", "⊸", "\\multimap")  -- Unicode only
  , ("parallel array open", "(|", "⦇", "\\llparenthesis")
  , ("parallel array close", "|)", "⦈", "\\rrparenthesis")
  , ("quasiquote open", "[|", "⟦", "\\llbracket")
  , ("quasiquote close", "|]", "⟧", "\\rrbracket")
  ]

runAllTests :: IO ()
runAllTests = do
  putStrLn "\n=== GHC UnicodeSyntax Operator Tests ==="
  mapM_ testOperatorPair unicodeOperatorTests
  putStrLn "All Unicode operator tests passed! ✓"
