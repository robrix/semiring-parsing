{-# LANGUAGE FunctionalDependencies #-}
-- | A left- or right-semimodule over a 'Semiring' generalizes the concept of a vector space over a field.
module Data.Semimodule
( LeftSemimodule(..)
) where

import Data.Semiring

class (Semiring r, Monoid m) => LeftSemimodule r m | m -> r where
  (><<) :: r -> m -> m
  infixr 7 ><<
