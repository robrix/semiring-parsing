{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Map.Modular
( Map(..)
) where

import           Data.Indexable
import qualified Data.Map as Map

newtype Map k v = Map { getMap :: Map.Map k v }

instance (Ord k, Monoid v) => Indexable k v (Map k v) where
  m ! k = Map.findWithDefault mempty k (getMap m)

instance (Ord k, Monoid v) => Singleton k v (Map k v) where
  (|->) = fmap Map . Map.singleton

instance (Ord k, Semigroup v) => Semigroup (Map k v) where
  Map a <> Map b = Map (Map.unionWith (<>) a b)

instance (Ord k, Monoid v) => Monoid (Map k v) where
  mempty = Map Map.empty
