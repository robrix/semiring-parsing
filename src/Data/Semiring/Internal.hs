{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Semiring.Internal
( zero
, IsZero(..)
, Splittable(..)
, Semiring(..)
, Unital(..)
, IsOne(..)
, Star(..)
, Key
, Indexable(..)
, Singleton(..)
, single
, value
) where

import           Control.Applicative (liftA2)
import           Data.Functor.Const
import           Data.Functor.Identity
import           Data.Kind (Type)
import           Data.Monoid (Sum(..))
import qualified Data.Set as Set

-- | The zero of a 'Monoid', defined as a synonym for 'mempty'.
zero :: Monoid a => a
zero = mempty


class Monoid a => IsZero a where
  isZero :: a -> Bool

deriving instance IsZero r => IsZero (Const r a)
deriving instance IsZero r => IsZero (Identity r)

instance IsZero () where
  isZero _ = True

instance (IsZero a, IsZero b) => IsZero (a, b) where
  isZero (a, b) = isZero a && isZero b

instance (IsZero a, IsZero b, IsZero c) => IsZero (a, b, c) where
  isZero (a, b, c) = isZero a && isZero b && isZero c

instance (IsZero a, IsZero b, IsZero c, IsZero d) => IsZero (a, b, c, d) where
  isZero (a, b, c, d) = isZero a && isZero b && isZero c && isZero d

instance Ord a => IsZero (Set.Set a) where
  isZero = Set.null


-- | 'Splittable' 'Monoid's have an additional 'splits' operation, a multi-valued inverse of '<>'.
class Monoid a => Splittable a where
  splits :: a -> [(a, a)]

instance (Enum a, Num a) => Splittable (Sum a) where
  splits n = [ (Sum i, n - Sum i) | i <- [0..getSum n] ]


-- | A 'Semiring' is a commutative 'Semigroup' with an additional associative operation, '><', which distributes over '<>'. E.g. if '<>' is “addition,” then '><' is “multiplication.”
--
-- Commutativity of '<>':
--
-- @
-- a '<>' b = b '<>' a
-- @
--
-- Associativity of '><':
--
-- @
-- a '><' (b '><' c) = (a '><' b) '><' c
-- @
--
-- Left- and right-distributivity of '><' over '<>':
--
-- @
-- a '><' (b '<>' c) = a '><' b '<>' a '><' c
-- @
-- @
-- (a '<>' b) '><' c = a '><' c '<>' b '><' c
-- @
--
-- If @r@ is a 'Monoid', then 'mempty' must be a left- and right-annihilator for '><':
--
-- @
-- 'mempty' '><' a = 'mempty'
-- @
-- @
-- a '><' 'mempty' = 'mempty'
-- @
class Semigroup r => Semiring r where
  (><) :: r -> r -> r
  infixr 7 ><

deriving instance Semiring r => Semiring (Const r a)
deriving instance Semiring r => Semiring (Identity r)

instance Semiring () where
  _ >< _ = ()

instance (Semiring a, Semiring b) => Semiring (a, b) where
  (a1, b1) >< (a2, b2) = (a1 >< a2, b1 >< b2)

instance (Semiring a, Semiring b, Semiring c) => Semiring (a, b, c) where
  (a1, b1, c1) >< (a2, b2, c2) = (a1 >< a2, b1 >< b2, c1 >< c2)

instance (Semiring a, Semiring b, Semiring c, Semiring d) => Semiring (a, b, c, d) where
  (a1, b1, c1, d1) >< (a2, b2, c2, d2) = (a1 >< a2, b1 >< b2, c1 >< c2, d1 >< d2)

instance Semiring b => Semiring (a -> b) where
  (><) = liftA2 (><)

instance (Monoid a, Ord a) => Semiring (Set.Set a) where
  p >< q = Set.fromList
    [ u <> v
    | u <- Set.toList p
    , v <- Set.toList q
    ]


-- | 'Unital' 'Semiring's are 'Monoid'al 'Semiring's with an addiitonal constant 'one' serving as the left- and right-identity of '><'.
--
-- @
-- 'one' '><' a = a
-- @
-- @
-- a '><' 'one' = a
-- @
class (Monoid r, Semiring r) => Unital r where
  one :: r

deriving instance Unital r => Unital (Const r a)
deriving instance Unital r => Unital (Identity r)

instance Unital () where
  one = ()

instance (Unital a, Unital b) => Unital (a, b) where
  one = (one, one)

instance (Unital a, Unital b, Unital c) => Unital (a, b, c) where
  one = (one, one, one)

instance (Unital a, Unital b, Unital c, Unital d) => Unital (a, b, c, d) where
  one = (one, one, one, one)

instance Unital b => Unital (a -> b) where
  one = const one

instance (Monoid a, Ord a) => Unital (Set.Set a) where
  one = Set.singleton mempty


class (IsZero r, Unital r) => IsOne r where
  isOne :: r -> Bool

deriving instance IsOne r => IsOne (Const r a)
deriving instance IsOne r => IsOne (Identity r)

instance IsOne () where
  isOne _ = True

instance (IsOne a, IsOne b) => IsOne (a, b) where
  isOne (a, b) = isOne a && isOne b

instance (IsOne a, IsOne b, IsOne c) => IsOne (a, b, c) where
  isOne (a, b, c) = isOne a && isOne b && isOne c

instance (IsOne a, IsOne b, IsOne c, IsOne d) => IsOne (a, b, c, d) where
  isOne (a, b, c, d) = isOne a && isOne b && isOne c && isOne d

instance (IsZero a, Ord a) => IsOne (Set.Set a) where
  isOne s = case Set.toList s of
    [a] | isZero a -> True
    _              -> False


-- | Star 'Semiring's are 'Unital' semirings admitting infinite combinations via the Kleene 'star' (or closure) operation.
--
-- @
-- 'star' p = 'one' '<>' p '><' 'star' p
-- @
class Unital r => Star r where
  star :: r -> r
  star p = one <> p >< star p

deriving instance Star r => Star (Const r a)
deriving instance Star r => Star (Identity r)

instance Star () where
  star _ = ()

instance (Star a, Star b) => Star (a, b) where
  star (a, b) = (star a, star b)

instance (Star a, Star b, Star c) => Star (a, b, c) where
  star (a, b, c) = (star a, star b, star c)

instance (Star a, Star b, Star c, Star d) => Star (a, b, c, d) where
  star (a, b, c, d) = (star a, star b, star c, star d)

instance Star b => Star (a -> b)

instance (Monoid a, Ord a) => Star (Set.Set a)


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
