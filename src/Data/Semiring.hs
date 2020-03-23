{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
-- | Semirings of various flavours.
module Data.Semiring
( zero
, IsZero(..)
, Semiring(..)
, Unital(..)
, IsOne(..)
, Closed(..)
  -- * Concrete semirings
, Arith(..)
, Few(..)
, Count(..)
, Boolean(..)
) where

import           Control.Applicative (liftA2)
import           Data.Coerce (coerce)
import           Data.Functor.Const
import           Data.Functor.Identity
import           Data.Ix
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


-- | Closed 'Semiring's are 'Unital' semirings admitting infinite combinations via the Kleene 'closure' (or closure) operation.
--
-- @
-- 'closure' p = 'one' '<>' p '><' 'closure' p
-- @
class Unital r => Closed r where
  closure :: r -> r
  closure p = one <> p >< closure p

deriving instance Closed r => Closed (Const r a)
deriving instance Closed r => Closed (Identity r)

instance Closed () where
  closure _ = ()

instance (Closed a, Closed b) => Closed (a, b) where
  closure (a, b) = (closure a, closure b)

instance (Closed a, Closed b, Closed c) => Closed (a, b, c) where
  closure (a, b, c) = (closure a, closure b, closure c)

instance (Closed a, Closed b, Closed c, Closed d) => Closed (a, b, c, d) where
  closure (a, b, c, d) = (closure a, closure b, closure c, closure d)

instance Closed b => Closed (a -> b)

instance (Monoid a, Ord a) => Closed (Set.Set a)


newtype Arith a = Arith { getArith :: a }
  deriving (Bounded, Enum, Eq, Ix, Foldable, Functor, Num, Ord, Read, Show, Traversable)

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

instance Closed Few where
  closure Zero = one
  closure _    = More


data Count a
  = Finite a
  | Infinity
  deriving (Eq, Foldable, Functor, Ord, Read, Show, Traversable)

instance Semigroup a => Semigroup (Count a) where
  Finite a <> Finite b = Finite (a <> b)
  _        <> _        = Infinity

instance Monoid a => Monoid (Count a) where
  mempty = Finite mempty

instance Semiring a => Semiring (Count a) where
  Finite a >< Finite b = Finite (a >< b)
  _        >< _        = Infinity

instance Unital a => Unital (Count a) where
  one = Finite one

instance (IsZero a, Unital a) => Closed (Count a) where
  closure (Finite a) | isZero a = one
  closure _                     = Infinity


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

instance IsOne Boolean where
  isOne = (== one)

instance Closed Boolean where
  closure _ = one
