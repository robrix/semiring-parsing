{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
-- | Semirings of various flavours.
module Data.Semiring
( zero
, Semiring(..)
, Unital(..)
, Star(..)
  -- * Zero semiring
, Zero(..)
) where

import Data.Functor.Const
import Data.Functor.Identity

-- | The zero of a 'Monoid', defined as a synonym for 'mempty'.
zero :: Monoid a => a
zero = mempty


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


newtype Zero = Zero { isZero :: Bool }
