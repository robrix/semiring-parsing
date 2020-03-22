-- | Semirings of various flavours.
module Data.Semiring
( zero
, Semiring(..)
, Unital(..)
) where

zero :: Monoid a => a
zero = mempty


-- | A 'Semiring' is a commutative 'Semigroup' with an additional associative operation, '><', which distributes over '<>'. E.g. if '<>' is “addition,” then '><' is “multiplication.”
class Semigroup r => Semiring r where
  (><) :: r -> r -> r
  infixr 7 ><


class (Monoid r, Semiring r) => Unital r where
  one :: r
