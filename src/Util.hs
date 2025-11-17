{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
-- | Shared utilities that may be moved to upstream libraries.
module Util where

import           Data.Function(on)
import           Data.List(groupBy, sortBy)
import           Data.Maybe(fromMaybe)
import qualified Data.Text as T
import           Optics.Core ( Lens, lens )
import           Prelude hiding(getLine)

import           Token(MyTok(..))

-- | Lens with default value.
maybeLens :: a -> Lens (Maybe a) (Maybe a1) a a1
maybeLens dflt = lens (fromMaybe dflt) (\_ a -> Just a)

-- | Sort and group inputs by a given `Ordering`.
grouping    :: Ord k
            => (   a -> k)
            ->    [a]
            ->   [[a]]
grouping key = groupBy ((==)    `on` key)
             . sortBy  (compare `on` key)

-- | Sort and remove duplicates in a list.
--   Duplicates are detected as equals by default `Ord`ering.
nubSorted :: Ord a => [a] -> [a]
nubSorted  = nubSortedBy compare

-- | Given an `Ord`ering, sort and remove duplicates.
nubSortedBy :: (a -> a -> Ordering) -> [a] -> [a]
nubSortedBy comparison = fmap    head
                       . groupBy equality
                       . sortBy  comparison
  where
    equality a b = a `comparison` b == EQ

-- | Safe tail function that returns empty list for empty input.
safeTail []     = []
safeTail (_:ls) = ls

-- | Take text in braces, and return its inner part.
--   Fail if the given text does not start with opening brace,
--   and end in closing brace.
unbrace txt | T.head txt == '(' && T.last txt == ')' && T.length txt > 2 = Just $ T.tail $ T.init txt
unbrace _ = Nothing

brace txt = mconcat ["(", txt, ")"]

-- | Preprocess tokens before formatting
--   in order to detect tokens like functions converted to operator syntax.
--   These are merged into a single token.
preformatTokens []                                                     = []
preformatTokens ((TOperator,"`"):(TVar, "elem"):(TOperator, "`"):rest) = (TOperator, "elem"):preformatTokens rest
preformatTokens (a                                              :rest) =  a                 :preformatTokens rest

