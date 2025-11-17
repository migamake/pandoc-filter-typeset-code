{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE NamedFieldPuns    #-}
module Main where

import           Text.Pandoc.JSON
import           Text.Pandoc.Walk       (walk)
import           Text.Pandoc.Definition ()
import           Data.String            (IsString)
import           Data.Maybe             (fromMaybe)
import qualified Data.Map  as Map
import           Data.Text(Text)
import qualified Data.Text as T

import qualified Token.Haskell          (tokenizer)
import qualified Token.Skylighting
import           Filter                 (renderBlock, renderInline)
import           FindColumns            (findColumns)
import           Alignment              (Processed)
import           Render.Latex           (latexPackages)
import           System.IO
import Debug.Trace(trace)

newtype Options = Options {
    inlineSyntax :: Text
  } deriving (Show)

main :: IO ()
main = toJSONFilter runner

-- https://www.logicmatters.net/latex-for-logicians/symbols/
-- Just (MetaList [MetaBlocks [RawBlock (Format "tex") "\\usepackage{scalerel}"]])

-- | Main body of Pandoc filter.
--   Reads format option, metadata,
--   and calls `walk` with `blockFormatter`.
runner :: Maybe Format -> Pandoc -> IO Pandoc
runner (fromMaybe (Format "text") -> format) input@(Pandoc (Meta meta) ast) =
  do
    hPutStrLn stderr $ "Output format: " <> show format
    hPrint stderr opts
    hPrint stderr (Map.lookup "header-includes" meta)
    let top = if format `elem` [Format "latex", Format "tex", Format "beamer"]
                 then Pandoc (Meta $ Map.alter modifyIncludes "header-includes" meta) ast
                 else input
    return $ walk (blockFormatter opts format) top
  where
    -- | Check default syntax for inline code
    opts = case Map.lookup "inline-code" meta of
      Nothing                    -> Options "haskell" -- default
      Just (MetaString       s ) -> Options  s -- never needed?
      Just (MetaInlines [Str s]) -> Options  s
      Just  otherValue           -> error $ "inline-code: meta should be a string but is: " <> show otherValue

-- | Add package dependencies to the meta "header-includes".
modifyIncludes :: Maybe MetaValue -> Maybe MetaValue
modifyIncludes = Just
               . addTeXPackages latexPackages
               . fromMaybe (MetaList [])
    -- Just (MetaBlocks [RawBlock (Format "tex") s])
  where
    addTeXPackages :: [Text] -> MetaValue -> MetaValue
    addTeXPackages = addTeXInclude
                   . T.unlines  
                   . fmap (\name -> "\\usepackage{" <> name <> "}")
    addTeXInclude :: Text -> MetaValue -> MetaValue
    addTeXInclude rawTeX (MetaList ls) = -- this variant is invalid for std templates
      MetaList  $ MetaBlocks [RawBlock (Format "tex") rawTeX]:ls
    addTeXInclude rawTeX (MetaBlocks [RawBlock "tex" s]) = -- pandoc-types>=1.22
      -- trace ("adding pandoc-types<1.20 (" <> T.unpack rawTeX <> ")") $
      MetaBlocks [RawBlock "tex" $ mconcat [s, "\n", rawTeX]]
    addTeXInclude rawTeX (MetaBlocks [RawBlock "latex" s]) = -- old pandoc-types
      -- trace ("adding pandoc-types<1.20 (" <> T.unpack rawTeX <> ")") $
      MetaBlocks [RawBlock "tex" $ mconcat [s, "\n", rawTeX]]
    addTeXInclude rawTeX other = error $ "Add a new case for addTeXInclude " <> show other

-- | Select the desired format output then process it.
--   Run tokenizer, analysis, and formatter.
--
--   For non-CodeBlocks it runs `inlineFormatter`.
--   Fallback to original input, if failed to tokenize.
blockFormatter :: Options
               -> Format -- ^ Output format, defaults to "plain" if not found
               -> Block
               -> Block
blockFormatter _opts format (CodeBlock attrs content) =
  case findColumns <$> getTokenizer attrs content of
    Just processed -> renderBlock format attrs processed
    Nothing        -> CodeBlock          attrs content -- fallback
-- Do not touch other blocks than 'CodeBock'
blockFormatter opts  format x                         = do
    walk (inlineFormatter opts format) x

-- | Select the desired format output then process it.
--   Run tokenizer, and inline formatter.
--   Fallback to original input, if failed to tokenize.
inlineFormatter :: Options
                -> Format -- ^ Output format, defaults to "plain" if not found
                -> Inline
                -> Inline
inlineFormatter Options {inlineSyntax} format (Code attrs txt) =
    case getTokenizer (addClass attrs) txt of
      Just processed -> renderInline format attrs
                      $ map discardLoc processed
      Nothing        -> Code                attrs txt -- fallback
  where
    discardLoc (a, _, b) = (a, b)
    addClass (a, classes, b) | inlineSyntax /= "" = (a, inlineSyntax:classes, b)
    addClass  o                                   = o
inlineFormatter _opts                  _      x   = x

-- | Pick tokenizer depending on options.
getTokenizer attrs | isHaskell attrs = Token.Haskell.tokenizer
getTokenizer (_,classes,_)           = case Token.Skylighting.lookupTokenizer classes of
                                         Just syntax -> Token.Skylighting.tokenizer syntax
                                         Nothing     -> \_ -> Nothing

-- | Check if the code block is tagged as Haskell.
isHaskell :: (Foldable t, Eq a1, IsString a1) =>
                   (a2, t a1, c) -> Bool
isHaskell (_, classes, _) = "haskell" `elem` classes
