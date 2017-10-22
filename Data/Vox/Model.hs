{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Vox.Model
  ( Model (..)
  , BoundedMap
  , Voxel
  , Point
  , Word8
  , V3 (..)
  ) where

import qualified Data.Array.IArray as IArray
import qualified Data.Map          as Map

import Data.Array.IArray  (Array)
import Data.Array.Unboxed (UArray)
import Data.Ix
import Data.Map           (Map)
import Data.Word          (Word8)
import Linear.V3          (V3 (..))

type Point = V3 Word8
type Voxel = (Point, Word8)

class Model (f :: * -> * -> *) where
  toVoxels   :: f Point Word8  -> [Voxel]
  fromVoxels :: (Point, Point) -> [Voxel] -> f Point Word8
  bounds     :: f Point Word8  -> (Point, Point)

instance Model UArray where
  toVoxels   = filter ((/= 0) . snd) . IArray.assocs
  fromVoxels = IArray.accumArray const 0
  bounds     = IArray.bounds

instance Model Array where
  toVoxels   = filter ((/= 0) . snd) . IArray.assocs
  fromVoxels = IArray.accumArray const 0
  bounds     = IArray.bounds

instance Model BoundedMap where
  toVoxels = filter ((/= 0) . snd) . Map.assocs . _bmap
  fromVoxels bnds = BoundedMap bnds .
    Map.fromList . filter (inRange bnds . fst)
  bounds = _bounds

data BoundedMap k v = BoundedMap {_bounds :: (k, k), _bmap :: Map k v}
  deriving (Eq, Ord, Show)
