{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleInstances      #-}
-- | Tuple utilities.
module Tuples where

-- | Class of tuples that can be expanded
--   by a single element.
class Annex c b a | c -> a, c -> b where
  annex :: b -> a -> c

instance Annex (a, b, c) (a, b) c where
  annex (a, b) c = (a, b, c)

instance Annex (a, b, c, d) (a, b, c) d where
  annex (a, b, c) d = (a, b, c, d)

instance Annex (a, b, c, d, e) (a, b, c, d) e where
  annex (a, b, c, d) e = (a, b, c, d, e)

instance Annex (a, b, c, d, e, f) (a, b, c, d, e) f where
  annex (a, b, c, d, e) f = (a, b, c, d, e, f)
