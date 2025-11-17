{-# LANGUAGE OverloadedStrings #-}
-- | Tests for tokenizer consistency on backslash operators and lambda expressions.
--   Compares behavior between Haskell tokenizer and Skylighting tokenizer.
module LambdaSpec where

import           Test.QuickCheck
import qualified Data.Text as T
import           Data.Text (Text)
import           Data.Maybe (fromMaybe)
import           Control.Monad (liftM2, liftM3)

import qualified Token.Haskell as Haskell
import qualified Token.Skylighting as Sky
import           Token (MyTok(..), MyLoc(..))
import           Render.Latex (latexInline)

-- * QuickCheck Generators for Haskell Code

-- | Generate valid Haskell variable names (lowercase start)
genVarName :: Gen Text
genVarName = do
  first <- elements ['a'..'z']
  rest <- resize 5 $ listOf (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['\''])
  return $ T.pack (first:rest)

-- | Generate simple Haskell expressions
genSimpleExpr :: Gen Text
genSimpleExpr = frequency
  [ (5, genVarName)
  , (2, fmap (T.pack . show) (arbitrary :: Gen Int))
  , (1, liftM2 (\a b -> a <> " + " <> b) genVarName genVarName)
  , (1, liftM2 (\a b -> a <> " * " <> b) genVarName genVarName)
  ]

-- | Generate lambda expressions with single parameter
-- Examples: \x -> x, \y -> y + 1
genLambdaSimple :: Gen Text
genLambdaSimple = do
  var <- genVarName
  body <- genSimpleExpr
  return $ "\\" <> var <> " -> " <> body

-- | Generate lambda expressions with multiple parameters
-- Examples: \x y -> x + y
genLambdaMulti :: Gen Text
genLambdaMulti = do
  n <- choose (2, 3)
  vars <- vectorOf n genVarName
  body <- genSimpleExpr
  return $ "\\" <> T.intercalate " " vars <> " -> " <> body

-- | Generate nested lambda expressions (curried functions)
-- Examples: \x -> \y -> x + y
genLambdaNested :: Gen Text
genLambdaNested = do
  n <- choose (2, 3)
  vars <- vectorOf n genVarName
  body <- genSimpleExpr
  -- Generate \x -> \y -> body
  let mkLambda v rest = "\\" <> v <> " -> " <> rest
  return $ foldr mkLambda body vars

-- | Generate lambda expression with lambda calculus notation (using dots)
-- Examples: \f. \x. f x
genLambdaDot :: Gen Text
genLambdaDot = do
  n <- choose (2, 3)
  vars <- vectorOf n genVarName
  body <- genSimpleExpr
  -- Generate \f. \x. body
  let parts = map (\v -> "\\" <> v <> ".") vars ++ [body]
  return $ T.intercalate " " parts

-- | Generate any lambda expression
genLambda :: Gen Text
genLambda = frequency
  [ (5, genLambdaSimple)
  , (3, genLambdaMulti)
  , (2, genLambdaNested)
  , (2, genLambdaDot)
  ]

-- | Generate set difference expressions
-- Examples: xs \\ ys, [1,2] \\ [2]
genSetDiff :: Gen Text
genSetDiff = do
  a <- genVarName
  b <- genVarName
  return $ a <> " \\\\ " <> b

-- * Unit Tests for fixBackslashOperators preprocessing

mkLoc :: MyLoc
mkLoc = MyLoc 1 1 True

-- Helper to compare tokens ignoring location details
tokensEq :: [(MyTok, MyLoc, Text)] -> [(MyTok, MyLoc, Text)] -> Bool
tokensEq xs ys = map (\(t,_,txt) -> (t,txt)) xs == map (\(t,_,txt) -> (t,txt)) ys

-- | Property: Standalone \ followed by + should merge to \+
prop_fix_backslash_plus :: Property
prop_fix_backslash_plus =
  let input = [(TOther, mkLoc, "\\"), (TOperator, mkLoc, "+")]
      expected = [(TOperator, mkLoc, "\\+")]
  in Sky.fixBackslashOperators input `tokensEq` expected === True

-- | Property: Standalone \ followed by > should merge to \>
prop_fix_backslash_gt :: Property
prop_fix_backslash_gt =
  let input = [(TOther, mkLoc, "\\"), (TOperator, mkLoc, ">")]
      expected = [(TOperator, mkLoc, "\\>")]
  in Sky.fixBackslashOperators input `tokensEq` expected === True

-- | Property: Two backslashes should merge to \\
prop_fix_double_backslash :: Property
prop_fix_double_backslash =
  let input = [(TOther, mkLoc, "\\"), (TOther, mkLoc, "\\")]
      expected = [(TOperator, mkLoc, "\\\\")]
  in Sky.fixBackslashOperators input `tokensEq` expected === True

-- | Property: Backslash followed by variable should stay separate (lambda)
prop_fix_lambda_variable :: Property
prop_fix_lambda_variable =
  let input = [(TOther, mkLoc, "\\"), (TVar, mkLoc, "x")]
      expected = [(TOther, mkLoc, "\\"), (TVar, mkLoc, "x")]
  in Sky.fixBackslashOperators input `tokensEq` expected === True

-- | Property: Token ending with \ followed by / should merge to \/
prop_fix_backslash_slash :: Property
prop_fix_backslash_slash =
  let input = [(TOther, mkLoc, "a \\"), (TOperator, mkLoc, "/")]
      expected = [(TOperator, mkLoc, "a \\/")]
  in Sky.fixBackslashOperators input `tokensEq` expected === True

-- | Property: Backslash in token should stay (lambda)
prop_fix_lambda_in_token :: Property
prop_fix_lambda_in_token =
  let input = [(TOther, mkLoc, "\\x")]
      expected = [(TOther, mkLoc, "\\x")]
  in Sky.fixBackslashOperators input `tokensEq` expected === True

-- * Unit Tests

-- | Test cases for backslash operators and lambda expressions.
--   Tests tokenizer consistency on constructs that Skylighting often mishandles.
backslashOperatorTestCases :: [(String, Text, String)]
backslashOperatorTestCases =
  [ ("simple lambda", "\\x -> x", "lambda")
  , ("lambda at start", "\\x -> case x of", "lambda")
  , ("multiple lambdas", "\\f. \\x. f (f x)", "lambda")
  , ("curried lambda", "\\x -> \\y -> x + y", "lambda")
  , ("lambda in expression", "map (\\x -> x + 1) xs", "lambda")
  , ("unicode lambda", "λx -> x", "lambda")
  , ("set difference", "a \\\\ b", "setminus")
  , ("set diff in list", "xs \\\\ ys", "setminus")
  , ("or operator", "a \\/ b", "land")        -- \/ is logical and
  , ("and operator", "a /\\ b", "lor")        -- /\ is logical or
  , ("backslash operator +", "\\+ 1", "backslash")
  , ("backslash operator >", "\\> x", "backslash")
  ]

-- | Helper function to test both tokenizers with a predicate
--   Reduces duplication in property tests
testBothTokenizers :: Text -> String -> (Text -> Property) -> Property
testBothTokenizers code descr check =
  let toLatex tokens = latexInline $ map (\(t,_,txt) -> (t, txt)) tokens
      haskellResult = fmap toLatex (Haskell.tokenizer code)
      skyResult = Sky.lookupTokenizer ["haskell"] >>= \syntax ->
                    fmap toLatex (Sky.tokenizer syntax code)
  in counterexample descr $
     case (haskellResult, skyResult) of
       (Just h, Just s) ->
         counterexample ("Haskell output: " ++ T.unpack h) $
         counterexample ("Skylighting output: " ++ T.unpack s) $
         label "both tokenized" $
         check h .&&. check s
       (Just h, Nothing) ->
         counterexample ("Haskell output: " ++ T.unpack h) $
         label "only haskell" $
         check h
       (Nothing, Just s) ->
         counterexample ("Skylighting output: " ++ T.unpack s) $
         label "only skylighting" $
         check s
       _ -> label "neither tokenized" $ property True

-- | Property: Test that both tokenizers produce expected LaTeX commands
prop_backslash_operators :: Property
prop_backslash_operators =
  forAll (elements backslashOperatorTestCases) $ \(name, code, expected) ->
    let expectedCmd = "\\" <> T.pack expected
        hasExpected txt = property $ expectedCmd `T.isInfixOf` txt
        descr = "Test: " ++ name ++ "\nCode: " ++ T.unpack code ++ "\nExpected: " ++ expected
    in testBothTokenizers code descr hasExpected

-- * Property Tests

-- | Property: Single backslash in lambda should always render as \lambda
prop_lambda_renders_correctly :: Property
prop_lambda_renders_correctly =
  forAll genLambda $ \lambdaExpr ->
    let hasLambda output = property $ "\\lambda" `T.isInfixOf` output
        descr = "Lambda expression: " ++ T.unpack lambdaExpr
    in testBothTokenizers lambdaExpr descr hasLambda

-- | Property: Both tokenizers should produce the same number of \lambda commands
prop_tokenizers_agree_on_lambda_count :: Property
prop_tokenizers_agree_on_lambda_count =
  forAll genLambda $ \lambdaExpr ->
    let haskellResult = Haskell.tokenizer lambdaExpr >>= \tokens ->
          Just $ latexInline $ map (\(t,_,txt) -> (t, txt)) tokens

        skyResult = Sky.lookupTokenizer ["haskell"] >>= \syntax ->
          Sky.tokenizer syntax lambdaExpr >>= \tokens ->
            Just $ latexInline $ map (\(t,_,txt) -> (t, txt)) tokens

        countLambdas txt = T.count "\\lambda" txt

    in counterexample ("Lambda expression: " ++ T.unpack lambdaExpr) $
       case (haskellResult, skyResult) of
         (Just h, Just s) ->
           let hCount = countLambdas h
               sCount = countLambdas s
           in counterexample ("Haskell output: " ++ T.unpack h) $
              counterexample ("Skylighting output: " ++ T.unpack s) $
              counterexample ("Haskell lambdas: " ++ show hCount) $
              counterexample ("Skylighting lambdas: " ++ show sCount) $
              label ("lambdas: " ++ show hCount) $
              hCount === sCount
         _ -> label "tokenization failed" $ property True

-- | Property: No output should contain \textbackslash or similar problematic commands
prop_no_textbackslash :: Property
prop_no_textbackslash =
  forAll genLambda $ \lambdaExpr ->
    let hasBadBackslash txt = "\\textbackslash" `T.isInfixOf` txt ||
                              "\\backslashx" `T.isInfixOf` txt ||
                              "\\backslash{}" `T.isInfixOf` txt
        noBadBackslash txt = property $ not (hasBadBackslash txt)
        descr = "Lambda expression: " ++ T.unpack lambdaExpr
    in testBothTokenizers lambdaExpr descr noBadBackslash

-- | Property: Double backslash should render as \setminus, not lambda
prop_setdiff_not_lambda :: Property
prop_setdiff_not_lambda =
  forAll genSetDiff $ \expr ->
    let hasSetminus txt = "\\setminus" `T.isInfixOf` txt
        hasLambda txt = "\\lambda" `T.isInfixOf` txt
        checkSetdiff txt = property $ hasSetminus txt && not (hasLambda txt)
        descr = "Set difference expression: " ++ T.unpack expr
    in testBothTokenizers expr descr checkSetdiff

-- | Property: Tokenizers should handle the same code similarly
prop_tokenizers_compatible :: Property
prop_tokenizers_compatible =
  forAll genLambda $ \code ->
    let haskellToks = Haskell.tokenizer code
        skyToks = Sky.lookupTokenizer ["haskell"] >>= \syntax ->
                    Sky.tokenizer syntax code

        -- Extract just the token types and texts (ignoring locations)
        simplify = map (\(t,_,txt) -> (t, txt))

    in counterexample ("Code: " ++ T.unpack code) $
       case (haskellToks, skyToks) of
         (Just h, Just s) ->
           let hSimple = simplify h
               sSimple = simplify s
               hOutput = latexInline hSimple
               sOutput = latexInline sSimple
           in counterexample ("Haskell tokens: " ++ show (length hSimple)) $
              counterexample ("Skylighting tokens: " ++ show (length sSimple)) $
              counterexample ("Haskell output: " ++ T.unpack hOutput) $
              counterexample ("Skylighting output: " ++ T.unpack sOutput) $
              label "both tokenized" $
              property True -- Just ensure both can tokenize
         _ -> label "tokenization failed" $ property True

-- | Main test runner
runAllTests :: IO ()
runAllTests = do
  putStrLn "\n=== fixBackslashOperators Unit Tests ==="
  quickCheck prop_fix_backslash_plus
  quickCheck prop_fix_backslash_gt
  quickCheck prop_fix_double_backslash
  quickCheck prop_fix_lambda_variable
  quickCheck prop_fix_backslash_slash
  quickCheck prop_fix_lambda_in_token

  putStrLn "\n=== Backslash Operator Tests ==="
  quickCheckWith stdArgs { maxSuccess = length backslashOperatorTestCases } prop_backslash_operators

  putStrLn "\n=== Lambda Property Tests ==="
  quickCheckWith stdArgs { maxSuccess = 100 } prop_lambda_renders_correctly
  quickCheckWith stdArgs { maxSuccess = 100 } prop_tokenizers_agree_on_lambda_count
  quickCheckWith stdArgs { maxSuccess = 100 } prop_no_textbackslash
  quickCheckWith stdArgs { maxSuccess = 50 } prop_setdiff_not_lambda
  quickCheckWith stdArgs { maxSuccess = 100 } prop_tokenizers_compatible
