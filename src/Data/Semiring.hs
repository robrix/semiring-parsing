module Data.Semiring
( zero
, Semiring(..)
, Unital(..)
) where

zero :: Monoid a => a
zero = mempty


class Semigroup r => Semiring r where
  (><) :: r -> r -> r
  infixr 7 ><


class (Monoid r, Semiring r) => Unital r where
  one :: r
