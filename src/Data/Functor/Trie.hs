{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Functor.Trie
( Trie(..)
) where

import Data.Semimodule
import Data.Semiring

data Trie i a = a :< i (Trie i a)
  deriving (Foldable, Functor, Traversable)

infixr 5 :<

instance (Semigroup a, Semigroup (i (Trie i a))) => Semigroup (Trie i a) where
  (h1 :< t1) <> (h2 :< t2) = h1 <> h2 :< t1 <> t2

instance (Monoid a, Monoid (i (Trie i a))) => Monoid (Trie i a) where
  mempty = mempty :< mempty

instance (Monoid a, Semiring a, Functor i, Monoid (i (Trie i a))) => LeftSemimodule a (Trie i a) where
  s ><< (h :< t) = s >< h :< fmap (s ><<) t
