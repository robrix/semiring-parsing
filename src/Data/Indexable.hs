{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Indexable
( Key
, Indexable(..)
, Singleton(..)
, single
, value
) where

import           Data.Kind (Type)
import           Data.Semiring
import qualified Data.Set as Set

type family Key (f :: Type -> Type) :: Type

type instance Key ((->) k) = k

class Indexable a b x | x -> a b where
  (!) :: x -> a -> b
  infixl 9 !

instance Indexable a b (a -> b) where
  (!) = ($)

instance Ord a => Indexable a Bool (Set.Set a) where
  s ! a = Set.member a s


class Indexable a b x => Singleton a b x where
  (|->) :: a -> b -> x
  infixr 2 |->

instance (Eq a, Monoid b) => Singleton a b (a -> b) where
  a |-> b = \ a' -> if a == a' then b else mempty

instance Ord a => Singleton a Bool (Set.Set a) where
  a |-> True  = Set.singleton a
  _ |-> False = Set.empty

single :: (Singleton a b x, Unital b) => a -> x
single = (|-> one)

value :: (Singleton a b x, Monoid a) => b -> x
value = (zero |->)
