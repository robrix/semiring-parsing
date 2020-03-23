{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Map.Modular
( Map(..)
) where

import           Data.Foldable (fold)
import           Data.Indexable
import           Data.Semimodule
import           Data.Semiring
import qualified Data.Map as Map

newtype Map k v = Map { getMap :: Map.Map k v }
  deriving (Functor)

instance (Ord k, Monoid v) => Indexable k v (Map k v) where
  m ! k = Map.findWithDefault mempty k (getMap m)

instance (Ord k, Monoid v) => Singleton k v (Map k v) where
  (|->) = fmap Map . Map.singleton

instance (Ord k, Semigroup v) => Semigroup (Map k v) where
  Map a <> Map b = Map (Map.unionWith (<>) a b)

instance (Ord k, Monoid v) => Monoid (Map k v) where
  mempty = Map Map.empty

instance (Ord k, Monoid v) => IsZero (Map k v) where
  isZero = Map.null . getMap

instance (Ord k, Monoid v, Semiring v) => LeftSemimodule v (Map k v) where
  mul v m = (v ><) <$> m

instance (Ord k, Semigroup k, Monoid v, Semiring v) => Semiring (Map k v) where
  p >< q = fold
    [ u <> v |-> p ! u >< q ! v
    | u <- Map.keys (getMap p)
    , v <- Map.keys (getMap q)
    ]

instance (Ord k, Monoid k, Unital v) => Unital (Map k v) where
  one = zero |-> one
