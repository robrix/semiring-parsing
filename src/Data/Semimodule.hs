{-# LANGUAGE FunctionalDependencies #-}
-- | A left- or right-semimodule over a 'Semiring' generalizes the concept of a vector space over a field.
module Data.Semimodule
( LeftSemimodule(..)
) where

import Data.Semiring

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
  (><<) :: r -> m -> m
  infixr 7 ><<
