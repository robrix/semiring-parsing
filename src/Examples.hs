{-# LANGUAGE FlexibleContexts #-}
module Examples
( a
, b
, atoz
) where

import Data.Foldable (fold)
import Data.Indexable
import Data.Semiring

a, b :: (Singleton String b x, Unital b) => x
a = single "a"
b = single "b"

atoz :: (Singleton String b x, Unital b, Monoid x) => x
atoz = fold [ single [c] | c <- ['a'..'z'] ]
