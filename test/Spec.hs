{-# LANGUAGE OverloadedStrings #-}
-- | Test suite
module Main where

import           Control.Exception(assert)
import           Data.Text.Arbitrary
import           Test.QuickCheck
import qualified Data.Text as T
import           Data.Char
import           Optics.Core (view, Field1(_1))


import Token(MyTok(..)
            ,MyLoc(..)
            ,Tokenized)
import Token.Haskell(tokenizer)
import Render.Latex(subAndSuperscripts)
import qualified Render.Debug
import FindColumns
import Render.ColSpan
import Alignment(Processed)
import GHC.Stack

import qualified LambdaSpec

-- * Property tests for subAndSuperscripts

-- | Property: Single underscore should produce \textsubscript
prop_single_underscore_is_subscript :: Property
prop_single_underscore_is_subscript =
  forAll (listOf1 $ elements ['a'..'z']) $ \prefix ->
  forAll (listOf1 $ elements ['a'..'z']) $ \suffix ->
    let input = T.pack prefix <> "_" <> T.pack suffix
        output = subAndSuperscripts input
    in counterexample ("Input: " ++ T.unpack input) $
       counterexample ("Output: " ++ T.unpack output) $
       "\\textsubscript{" `T.isInfixOf` output

-- | Property: Double underscore should produce \textsuperscript
prop_double_underscore_is_superscript :: Property
prop_double_underscore_is_superscript =
  forAll (listOf1 $ elements ['a'..'z']) $ \prefix ->
  forAll (listOf1 $ elements ['a'..'z']) $ \suffix ->
    let input = T.pack prefix <> "__" <> T.pack suffix
        output = subAndSuperscripts input
    in counterexample ("Input: " ++ T.unpack input) $
       counterexample ("Output: " ++ T.unpack output) $
       "\\textsuperscript{" `T.isInfixOf` output

-- | Property: Output should not contain raw underscores (except escaped \_)
prop_no_raw_underscores :: Property
prop_no_raw_underscores =
  forAll (resize 20 $ listOf (elements (['a'..'z'] ++ ['_']))) $ \str ->
    not (null str) && str /= "_" ==>
      let input = T.pack str
          output = subAndSuperscripts input
          -- Count underscores that aren't part of \_ escape
          hasRawUnderscore = T.any (== '_') $ T.replace "\\_" "" output
      in counterexample ("Input: " ++ T.unpack input) $
         counterexample ("Output: " ++ T.unpack output) $
         label "processed" $
         not hasRawUnderscore

-- | Property: Empty input or single underscore should be handled correctly
prop_edge_cases :: Property
prop_edge_cases = conjoin
  [ subAndSuperscripts "" === " "
  , subAndSuperscripts "_" === "\\_"
  ]

prop_tokenizer str = case tokenizer str of
                       Nothing -> label "cannot lex" $ True
                       Just t  -> label "lexed" $ length t <= T.length str

prop_debug_renderer_text_length input =
    case debug of
      Nothing -> label "cannot lex" $ True
      Just  t -> label "lexed"
               $ all (uncurry (<=))
               $ zip (extract input)
                     (extract t)
  where
    extract :: T.Text -> [Int]
    extract = fmap T.length . T.lines
    debug :: Maybe T.Text
    debug = tokenizer input >>= (return . Render.Debug.render . findColumns)

prop_colspans input =
    case debug of
      Nothing -> label "cannot lex" $ True
      Just  t -> label "lexed" $
                 sameNumber t
  where
    extract :: T.Text -> [Int]
    extract = fmap T.length . T.lines
    debug :: Maybe [Int]
    debug = tokenizer input >>= (return . sumColSpans . findColumns)
    sameNumber [] = True
    sameNumber (n:ns) = all (n==) ns

prop_tableColumns input = T.any (not . Data.Char.isSpace) input ==>
  case tokenizer input of
    Nothing     -> label "cannot lex" $ True
    Just tokens | all ((TBlank==) . view _1) tokens
                -> label "only blanks" $ True
    Just tokens -> 
      case findColumns tokens of
        [] -> label "empty" $ True
        t  -> case sumColSpans t of
                []  -> label "no colspans"  $ True
                c:_ -> label "tableColumns" $ c == length (tableColumns t)
  where
    extract :: T.Text -> [Int]
    extract = fmap T.length . T.lines
    debug :: Maybe [Processed]
    debug = tokenizer input >>= (return . findColumns)
    sameNumber [] = True
    sameNumber (n:ns) = all (n==) ns

shouldBe :: (HasCallStack, Eq a) => a -> a -> IO ()
a `shouldBe` b = do
  if a /= b 
    then error "Inequal"
    else return ()

main :: IO ()
main = do
    putStrLn "=== Subscript/Superscript Tests ==="
    (subAndSuperscripts "alpha_beta")        `shouldBe` "alpha\\textsubscript{beta}"
    (subAndSuperscripts "alpha__beta")       `shouldBe` "alpha\\textsuperscript{beta}"
    (subAndSuperscripts "alpha__gamma_beta") `shouldBe` "alpha\\textsuperscript{gamma\\textsubscript{beta}}"

    putStrLn "\n=== Example Problems ==="
    problem " a\na"
    problem "--"
    problem "a\n a"
    problem "\n a"   -- prop_tableColumns failure case 1
    problem " a\na"  -- prop_tableColumns failure case 2 (duplicate but documented)

    putStrLn "\n=== Lambda Expression Tests ==="
    LambdaSpec.runAllTests

    putStrLn "\n=== SubAndSuperscripts Property Tests ==="
    putStrLn "  Testing single underscore → subscript"
    quickCheckWith stdArgs { maxSuccess = 100 } prop_single_underscore_is_subscript
    putStrLn "  Testing double underscore → superscript"
    quickCheckWith stdArgs { maxSuccess = 100 } prop_double_underscore_is_superscript
    putStrLn "  Testing no raw underscores in output"
    quickCheckWith stdArgs { maxSuccess = 100 } prop_no_raw_underscores
    putStrLn "  Testing edge cases"
    quickCheck prop_edge_cases

    putStrLn "\n=== General Property Tests ==="
    quickCheck   prop_tokenizer
    quickCheck   prop_debug_renderer_text_length
    quickCheck $ withMaxSuccess 10000  prop_colspans -- is it sufficient?
    quickCheck $ withMaxSuccess 100000 prop_tableColumns -- is it sufficient?
  where
    problem example = do
      putStrLn $ "Example: " <> show example
      print $ tokenizer example >>= (return . tableColumns . findColumns)
      print $ tokenizer example >>= (return . sumColSpans  . findColumns)
      print $ tokenizer example >>= (return . findColumns)
      print $ tokenizer example

sumColSpans = map (sum . map (\(_,c,_) -> c)) . colspans

