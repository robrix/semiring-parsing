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
