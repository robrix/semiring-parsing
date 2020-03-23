{-# LANGUAGE FlexibleContexts #-}
module Examples
( a
) where

import Data.Indexable
import Data.Semiring

a :: (Singleton String b x, Unital b) => x
a = single "a"
