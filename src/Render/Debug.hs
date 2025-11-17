{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
-- | Renders alignment to text with alignment markers,
--   for debugging purposes.
module Render.Debug(render, renderInline) where

import Data.Text (Text)
import qualified Data.Text as T
import Prelude hiding(getLine)
import Optics.Core ( view, Field2(..))

import FindColumns ( alignPos, getCol, tableColumns )
import Alignment ( textContent, Align(..), Processed )
import Util ( safeTail )

-- | Insert element at a given index of the list.
insertAt       :: Show a => Int -> a -> [a] -> [a]
insertAt i e ls = case maybeInsertAt i e ls of
                    Nothing -> error $ "Failed in insertAt " <> show i <> " " <> show e <> " " <> show ls
                    Just r  -> r

-- | Insert element at a given index of the list, or return error.
maybeInsertAt :: Int -> a -> [a] -> Maybe [a]
maybeInsertAt 0 e    ls  = pure $ e:ls
maybeInsertAt i e (l:ls) = (l:) <$> maybeInsertAt (i-1) e ls
maybeInsertAt _ _    []  = Nothing

-- | Render text of code blocks
--   with marker columns inserted
--   to indicate alignment boundaries.
render :: [Processed] -> Text
render ps = T.concat $ go ps
  where
    -- | Table columns between starting point of this segment and the next.
    lastColumn  = maximum tColumns + 1
    -- | Translates indices of alignment/table columns to text columns.
    tColumns :: [Int]
    tColumns = fst <$> tableColumns ps
    go  []        = []
    go (tok:toks) = alignMarker:textWithMarkers tColumns nextCol tok
                               :go toks
      where
        nextCol = case toks of
                    []       -> lastColumn
                    (next:_) -> getCol next
        alignMarker :: Text
        alignMarker  = case view alignPos tok of
                         Just (ACenter, _) -> "^"
                         Just _            -> "|"
                         _                 -> ""

-- | Text content with markers for markers inside it.
textWithMarkers tColumns nextCol tok = 
    T.pack $ insertMarkers unaccountedMarkers $ T.unpack $ view textContent tok
  where
    insertMarkers []   txt = txt
    insertMarkers mrks txt = foldr insertMarker txt mrks

    insertMarker index = insertAt index '.'
    unaccountedMarkers = fmap (-getCol tok+)
                       $ withoutKnownMarker
                       $ columnsBetween (getCol tok) nextCol
    withoutKnownMarker | Just _ <- view alignPos tok = safeTail
                       | otherwise                   = id
    columnsBetween :: Int -> Int -> [Int]
    columnsBetween colA colB = filter (\c -> c >= colA && c < colB) tColumns

renderInline :: Field2 a a Text Text => [a] -> Text
renderInline = mconcat . map (view _2)