{-# LANGUAGE FunctionalDependencies #-}
-- | A left- or right-semimodule over a 'Semiring' generalizes the concept of a vector space over a field.
module Data.Semimodule
( LeftSemimodule(..)
) where

import Data.Semiring

-- | Left-semimodules lift a 'Semiring'’s '><' operation to the semimodule’s elements.
class (Semiring r, Monoid m) => LeftSemimodule r m | m -> r where
  (><<) :: r -> m -> m
  infixr 7 ><<
