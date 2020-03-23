{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Functor.Trie
( Trie(..)
, (<|)
) where

import Data.Indexable
import Data.Semimodule
import Data.Semiring

data Trie i a = a :< i (Trie i a)
  deriving (Foldable, Functor, Traversable)

infix 1 :<

instance (Semigroup a, Semigroup (i (Trie i a))) => Semigroup (Trie i a) where
  (h1 :< t1) <> (h2 :< t2) = h1 <> h2 :< t1 <> t2

instance (Monoid a, Monoid (i (Trie i a))) => Monoid (Trie i a) where
  mempty = mempty :< mempty

instance (IsOne a, Functor i, Monoid (i (Trie i a))) => LeftSemimodule a (Trie i a) where
  mul s (h :< t) = s >< h :< fmap (s ><<) t

instance (IsOne a, Functor i, Monoid (i (Trie i a))) => Semiring (Trie i a) where
  (h :< t) >< q = h ><< q <> (zero :< fmap (>< q) t)

instance (IsOne a, Functor i, Monoid (i (Trie i a))) => Unital (Trie i a) where
  one = one :< zero

instance (IsOne a, Star a, Functor i, Monoid (i (Trie i a))) => Star (Trie i a) where
  star (h :< t) = q where q = star h ><< (one :< fmap (>< q) t)

instance (c ~ Key i, Monoid a, Monoid (i (Trie i a)), Singleton c (Trie i a) (i (Trie i a))) => Indexable [c] a (Trie i a) where
  (!) (b :< dp) = b <| (!) . (dp !)

instance (c ~ Key i, Monoid a, Monoid (i (Trie i a)), Singleton c (Trie i a) (i (Trie i a))) => Singleton [c] a (Trie i a) where
  w |-> b = foldr (\ c t -> zero :< (c |-> t)) (b :< zero) w


(<|) :: b -> (c -> ([c] -> b)) -> ([c] -> b)
b <| h = \case
  []   -> b
  c:cs -> h c cs

infix 1 <|
