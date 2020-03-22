{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Functor.Trie
( Trie(..)
) where

data Trie i a = a :< i (Trie i a)
  deriving (Foldable, Functor, Traversable)

infixr 5 :<

instance (Semigroup a, Semigroup (i (Trie i a))) => Semigroup (Trie i a) where
  (h1 :< t1) <> (h2 :< t2) = h1 <> h2 :< t1 <> t2
