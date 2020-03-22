{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
module Data.Indexable
( Indexable(..)
) where

class Indexable a b x | x -> a b where
  (!) :: x -> a -> b
  infixl 9 !

instance Indexable a b (a -> b) where
  (!) = ($)
