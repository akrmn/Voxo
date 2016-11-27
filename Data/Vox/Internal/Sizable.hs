{-# LANGUAGE NamedFieldPuns #-}

module Data.Vox.Internal.Sizable where

import           Data.Array.IArray (elems)
import           Data.Foldable     (foldr')
import           Data.Sequence     (Seq)
import           Data.Vox.Palette
import           Data.Vox.VoxData

class Sizable s where
  size :: s -> Int

instance Sizable Vox where
  size Vox { models, palette } =
    szModels + szPalette
    where
      szModels = size models        -- XYZI chunks' contents
               + length models * 36 -- XYZI headers + SIZE chunks (conts + headers)
               + 16 -- PACK chunk
      szPalette = size palette

instance Sizable Model where
  size (Model voxels) = (4*) . succ . length . filter (/= 0) . elems $ voxels

    where
      aux c = if c == 0 then id else succ
  -- We only count the bytes in the contents of the XYZI chunk

-- fmap (\x -> if x /= 0 then 1 else 0) $
instance Sizable Palette where
  size DefaultPalette   = 0
  size CustomPalette {} = 256 * 4 + 12
  -- We count both contents and header of the RGBA chunk

instance Sizable e => Sizable (Seq e) where
  size = sum . fmap size
