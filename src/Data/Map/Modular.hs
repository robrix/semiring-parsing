module Data.Map.Modular
( Map(..)
) where

import qualified Data.Map as Map

newtype Map k v = Map { getMap :: Map.Map k v }
