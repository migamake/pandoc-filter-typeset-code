{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
-- | Helper functions for taking a list of
--   processed records, and returning
--   the same list splitted into separate colspans.
module Render.ColSpan where

import Data.Function(on)
import Data.Text (Text)
import Data.List(groupBy, sortBy)
import Prelude hiding(getLine)
import Optics.Core ( Field1(_1), Field2(_2), view, (%))
import Control.Exception(assert)

import Alignment
    ( textContent, tokenType, Align(ALeft), Processed )
import FindColumns ( alignPos, getLine, getLineCol )
import Token(MyTok)
import Util ( maybeLens )

-- | A list of tokens to be put into a single colspan,
--   Number of table columns in this colspan, and alignment option.
type TokensWithColSpan = ([(MyTok, Text)], Int, Align)

-- | Find colspan parameters
colspans   :: [Processed] -> [[TokensWithColSpan]]
colspans ps = fmap ( fmap extractTokens -- extract token and text content from each record
                   . addColSpans -- add colspan length in alignment columns
                   . groupBy sameColSpan ) -- group colspans
            $ groupBy ((==)    `on` getLine) -- group lines
            $ sortBy  (compare `on` getLineCol) ps
  where
    maxCol :: Int
    maxCol = maximum $ map getAlignCol ps
    sameColSpan :: Processed -> Processed -> Bool
    sameColSpan tok1 tok2 = case view alignPos tok2 of
                              Nothing | getLine tok1 == getLine tok2 -> True
                              _                                      -> False
    addColSpans :: [[Processed]] -> [([Processed], Int, Align)]
    addColSpans []       = []
    addColSpans [a]      = [(a, maxCol -getAlignCol (head a)+1, getAlign $ head a)]
    addColSpans (b:c:cs) =  (b, nextCol-getAlignCol (head b)  , getAlign $ head b):addColSpans (c:cs)
      where
        nextCol = getAlignCol $ head c
    getAlign :: Processed -> Align
    getAlign = view (alignPos % maybeLens (ALeft, 1) % _1)
    extractTokens (a,b,c) = (extractToken <$> a, b, c)
    extractToken tok = (view tokenType tok, view textContent tok)
-- FIXME: split lines before colspans!

numColSpans :: [Processed] -> Int
numColSpans ps = case colspansPerLine of
                   []   -> 1
                   n:ns -> assert (all (n==) ns) n
  where
    colspansPerLine :: [Int]
    colspansPerLine = map (sum . map (\(_,c,_) -> c)) . colspans $ ps

-- | Given a `Processed` record, extract number of table columns.
getAlignCol :: Processed -> Int
getAlignCol = view (alignPos % maybeLens (ALeft, 0) % _2)

