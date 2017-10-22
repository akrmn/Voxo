module Data.Vox.VoxData
  ( Vox (..)
  , Point
  , Voxel
  , Word8
  ) where

import Data.Sequence    (Seq)
import Data.Vox.Model
import Data.Vox.Palette (Palette (..))

data Vox model = Vox
  { models  :: Seq (model Point Word8)
  , palette :: Palette }
