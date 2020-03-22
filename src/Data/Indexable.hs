{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Indexable
( Indexable(..)
, Singleton(..)
, single
, value
) where

import Data.Semiring
import Data.Kind (Type)

type family Key (f :: Type -> Type) :: Type

type instance Key ((->) k) = k

class Indexable a b x | x -> a b where
  (!) :: x -> a -> b
  infixl 9 !

instance Indexable a b (a -> b) where
  (!) = ($)


class Indexable a b x => Singleton a b x where
  (|->) :: a -> b -> x
  infixr 2 |->

instance (Eq a, Monoid b) => Singleton a b (a -> b) where
  a |-> b = \ a' -> if a == a' then b else mempty

single :: (Singleton a b x, Unital b) => a -> x
single = (|-> one)

value :: (Singleton a b x, Monoid a) => b -> x
value = (zero |->)
