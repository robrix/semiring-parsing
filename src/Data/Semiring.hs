-- | Semirings of various flavours.
module Data.Semiring
( zero
, Semiring(..)
, Unital(..)
, Star(..)
) where

-- | The zero of a 'Monoid', defined as a synonym for 'mempty'.
zero :: Monoid a => a
zero = mempty


-- | A 'Semiring' is a commutative 'Semigroup' with an additional associative operation, '><', which distributes over '<>'. E.g. if '<>' is “addition,” then '><' is “multiplication.”
--
-- Laws:
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


-- | Star 'Semiring's are 'Unital' semirings admitting infinite combinations via the Kleene 'star' (or closure) operation.
class Unital r => Star r where
  star :: r -> r
