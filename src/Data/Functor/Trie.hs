{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Functor.Trie
( Trie(..)
, (<|)
) where

import Data.Indexable
import Data.Semimodule
import Data.Semiring

data Trie i c a = a :< i c (Trie i c a)
  deriving (Foldable, Functor, Traversable)

infix 1 :<

instance (Semigroup a, Semigroup (i c (Trie i c a))) => Semigroup (Trie i c a) where
  (h1 :< t1) <> (h2 :< t2) = h1 <> h2 :< t1 <> t2

instance (Monoid a, Monoid (i c (Trie i c a))) => Monoid (Trie i c a) where
  mempty = mempty :< mempty

instance (IsZero a, IsZero (i c (Trie i c a))) => IsZero (Trie i c a) where
  isZero (h :< t) = isZero h && isZero t

instance (IsOne a, Functor (i c), Monoid (i c (Trie i c a))) => LeftSemimodule a (Trie i c a) where
  mul s (h :< t) = s >< h :< fmap (s ><<) t

instance (IsOne a, Functor (i c), Monoid (i c (Trie i c a))) => Semiring (Trie i c a) where
  (h :< t) >< q = h ><< q <> (zero :< fmap (>< q) t)

instance (IsOne a, Functor (i c), Monoid (i c (Trie i c a))) => Unital (Trie i c a) where
  one = one :< zero

instance (IsOne a, Functor (i c), IsZero (i c (Trie i c a))) => IsOne (Trie i c a) where
  isOne (h :< t) = isOne h && isZero t

instance (IsOne a, Star a, Functor (i c), Monoid (i c (Trie i c a))) => Star (Trie i c a) where
  star (h :< t) = q where q = star h ><< (one :< fmap (>< q) t)

instance Indexable c (Trie i c a) (i c (Trie i c a)) => Indexable [c] a (Trie i c a) where
  (!) (b :< dp) = b <| (!) . (dp !)

instance (Monoid a, Monoid (i c (Trie i c a)), Singleton c (Trie i c a) (i c (Trie i c a))) => Singleton [c] a (Trie i c a) where
  w |-> b = foldr (\ c t -> zero :< (c |-> t)) (b :< zero) w


(<|) :: b -> (c -> ([c] -> b)) -> ([c] -> b)
b <| h = \case
  []   -> b
  c:cs -> h c cs

infix 1 <|
