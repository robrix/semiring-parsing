{-# LANGUAGE FlexibleContexts #-}
module Examples
( a
, b
) where

import Data.Indexable
import Data.Semiring

a, b :: (Singleton String b x, Unital b) => x
a = single "a"
b = single "b"
