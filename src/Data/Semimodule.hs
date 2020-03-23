{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
-- | A left- or right-semimodule over a 'Semiring' generalizes the concept of a vector space over a field.
module Data.Semimodule
( LeftSemimodule(..)
, (><<)
) where

import           Data.Functor.Identity
import           Data.Semiring
import qualified Data.Set as Set

-- | Left-semimodules lift a 'Semiring'’s '><' operation to the semimodule’s elements.
--
-- Decomposition of '><':
--
-- @
-- (a '><' b) '><<' c = a '><<' b '><<' c
-- @
--
-- Left- and right-distributivity over '<>':
--
-- @
-- (a '<>' b) '><<' c = a '><<' c '<>' b '><<' c
-- @
-- @
-- a '><<' (b '<>' c) = a '><<' b '<>' a '><<' c
-- @
--
-- If @r@ is 'Monoid'al, we additionally have 'mempty' as a left-annihilator for '><<':
--
-- @
-- 'mempty' '><<' a = 'mempty'
-- @
--
-- If @r@ is 'Unital', we additionally have 'one' as a left-identity for '><<':
--
-- @
-- 'one' '><<' a = a
-- @
--
-- Finally, if @r@ is a commutative 'Semiring', @m@ is both a left- and right-semimodule over @r@, and their operations coincide.
class (Semiring r, Monoid m) => LeftSemimodule r m | m -> r where
  mul :: r -> m -> m

(><<) :: (LeftSemimodule r m, IsOne r) => r -> m -> m
r ><< m
  | isZero r  = zero
  | isOne  r  = m
  | otherwise = mul r m

infixr 7 ><<

instance (Monoid r, Semiring r) => LeftSemimodule r (a -> r) where
  mul a b = (a ><) <$> b

-- | Every 'Semiring' forms a 'LeftSemimodule' with itself, which we model using 'Identity' to avoid overlapping instances.
instance (Monoid r, Semiring r) => LeftSemimodule r (Identity r) where
  mul a b = (a ><) <$> b

instance LeftSemimodule () () where
  mul _ _ = ()

instance (LeftSemimodule r a, LeftSemimodule r b) => LeftSemimodule r (a, b) where
  mul a (b, c) = (mul a b, mul a c)

instance (LeftSemimodule r a, LeftSemimodule r b, LeftSemimodule r c) => LeftSemimodule r (a, b, c) where
  mul a (b, c, d) = (mul a b, mul a c, mul a d)

instance (LeftSemimodule r a, LeftSemimodule r b, LeftSemimodule r c, LeftSemimodule r d) => LeftSemimodule r (a, b, c, d) where
  mul a (b, c, d, e) = (mul a b, mul a c, mul a d, mul a e)

instance Semiring r => LeftSemimodule r [r] where
  mul a = map (a ><)

instance Ord a => LeftSemimodule Boolean (Set.Set a) where
  mul a b = if getBoolean a then b else zero
