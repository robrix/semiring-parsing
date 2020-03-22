module Data.Semiring
( zero
, Semiring(..)
) where

zero :: Monoid a => a
zero = mempty


class Semigroup r => Semiring r where
  (><) :: r -> r -> r
  infixr 7 ><
