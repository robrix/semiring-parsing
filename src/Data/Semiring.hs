{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

import Data.Coerce (coerce)
import Data.Ix
import Data.Semiring.Internal

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

instance IsOne Boolean where
  isOne = (== one)

instance Star Boolean where
  star _ = one
