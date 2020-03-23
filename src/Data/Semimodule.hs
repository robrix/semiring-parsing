{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
-- | A left- or right-semimodule over a 'Semiring' generalizes the concept of a vector space over a field.
module Data.Semimodule
( LeftSemimodule(..)
, Opt(..)
) where

import Data.Functor.Identity
import Data.Ix
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

instance (Monoid r, Semiring r) => LeftSemimodule r (a -> r) where
  a ><< b = (a ><) <$> b

-- | Every 'Semiring' forms a 'LeftSemimodule' with itself, which we model using 'Identity' to avoid overlapping instances.
instance (Monoid r, Semiring r) => LeftSemimodule r (Identity r) where
  a ><< b = (a ><) <$> b

instance LeftSemimodule () () where
  _ ><< _ = ()

instance (LeftSemimodule r a, LeftSemimodule r b) => LeftSemimodule r (a, b) where
  a ><< (b, c) = (a ><< b, a ><< c)

instance (LeftSemimodule r a, LeftSemimodule r b, LeftSemimodule r c) => LeftSemimodule r (a, b, c) where
  a ><< (b, c, d) = (a ><< b, a ><< c, a ><< d)

instance (LeftSemimodule r a, LeftSemimodule r b, LeftSemimodule r c, LeftSemimodule r d) => LeftSemimodule r (a, b, c, d) where
  a ><< (b, c, d, e) = (a ><< b, a ><< c, a ><< d, a ><< e)

instance Semiring r => LeftSemimodule r [r] where
  a ><< b = map (a ><) b


-- | Optimize another semimodule by applying the annihilation & identity laws.
newtype Opt a = Opt { getOpt :: a }
  deriving (Bounded, Enum, Eq, Ix, Functor, Monoid, Num, Ord, Read, Semigroup, Show)

instance LeftSemimodule r a => LeftSemimodule (Few, r) (Opt a) where
  (Zero, _) ><< _ = zero
  (One,  _) ><< b = b
  (_,    a) ><< b = Opt $ a ><< getOpt b
