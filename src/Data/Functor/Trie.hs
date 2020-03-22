module Data.Functor.Trie
( Trie(..)
) where

data Trie i a = a :< i (Trie i a)
