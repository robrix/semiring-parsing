{-# LANGUAGE FlexibleContexts #-}
module Examples
( a
, b
, atoz
, fishy
, anbn
) where

import Data.Foldable (fold)
import Data.Indexable
import Data.Semiring

a, b :: (Singleton String b x, Unital b) => x
a = single "a"
b = single "b"

atoz :: (Singleton String b x, Unital b, Monoid x) => x
atoz = fold [ single [c] | c <- ['a'..'z'] ]

fishy :: (Singleton String b x, Unital b, Star x) => x
fishy = star atoz >< single "fish" >< star atoz

anbn :: (Singleton String b x, Unital b, Unital x) => x
anbn = one <> a >< anbn >< b
