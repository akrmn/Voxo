module Data.Vox.VoxData
  ( Vox (..)
  , Model (..)
  , Point
  , Voxel
  ) where

import           Data.Array.Unboxed  (UArray)
import           Data.Binary      (Binary)
import           Data.Binary.Get  (getWord32le)
import           Data.Binary.Put  (putWord32le)
import           Data.Sequence    (Seq)
import           Data.Vox.Palette (Palette (..))
import           Data.Word        (Word8)

type Point = (Word8, Word8, Word8)

data Vox = Vox
  { models  :: Seq Model
  , palette :: Palette }
  deriving (Show)

newtype Model = Model (UArray Point Word8)
  deriving (Show)

type Voxel = (Point, Word8)
