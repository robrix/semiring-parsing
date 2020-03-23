{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
-- | Semirings of various flavours.
module Data.Semiring
( zero
, IsZero(..)
, Semiring(..)
, Unital(..)
, IsOne(..)
, Star(..)
  -- * Concrete semirings
, Arith(..)
, Few(..)
, Boolean(..)
) where

import Control.Applicative (liftA2)
import Data.Coerce (coerce)
import Data.Functor.Const
import Data.Functor.Identity
import Data.Ix

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


newtype Arith a = Arith { getArith :: a }
  deriving (Bounded, Enum, Eq, Ix, Functor, Num, Ord, Read, Show)

instance Num a => Semigroup (Arith a) where
  (<>) = (+)

instance Num a => Monoid (Arith a) where
  mempty = 0

instance (Eq a, Num a) => IsZero (Arith a) where
  isZero = (== 0)

instance Num a => Semiring (Arith a) where
  (><) = (*)

instance Num a => Unital (Arith a) where
  one = 1

instance (Eq a, Num a) => IsOne (Arith a) where
  isOne = (== 0)


data Few
  = Zero
  | One
  | More
  deriving (Bounded, Enum, Eq, Ix, Ord, Read, Show)

instance Semigroup Few where
  Zero <> b    = b
  a    <> Zero = a
  _    <> _    = More

instance Monoid Few where
  mempty = Zero

instance IsZero Few where
  isZero = (== Zero)

instance Semiring Few where
  Zero >< _    = Zero
  One  >< b    = b
  _    >< Zero = Zero
  a    >< One  = a
  _    >< _    = More

instance Unital Few where
  one = One

instance IsOne Few where
  isOne = (== One)

instance Star Few where
  star Zero = one
  star _    = More


newtype Boolean = Boolean { getBoolean :: Bool }
  deriving (Bounded, Enum, Eq, Ix, Ord, Read, Show)

instance Semigroup Boolean where
  (<>) = coerce (||)

instance Monoid Boolean where
  mempty = Boolean False

instance IsZero Boolean where
  isZero = (== zero)

instance Semiring Boolean where
  (><) = coerce (&&)

instance Unital Boolean where
  one = Boolean True

instance Star Boolean where
  star _ = one
