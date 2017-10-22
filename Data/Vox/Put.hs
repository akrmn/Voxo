{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Data.Vox.Put where

import Control.Monad             (forM_)
import Data.Binary.Put
import Data.Vox.Internal.Sizable (Sizable (..))
import Data.Vox.Palette          (Color (..), Palette (..))
import Data.Vox.VoxData          (Vox (..))
import Data.Vox.Model

putVox :: Model model => Vox model -> Put
putVox v@Vox { models, palette } = do
  putByteString "VOX "
  putInt 150 -- Version Number

  putByteString "MAIN"
  putInt 0
  putInt . size $ v

  putByteString "PACK"
  putInt 4
  putInt 0
  putInt . length $ models

  forM_ models $ \model -> do
    putByteString "SIZE"
    putInt 12
    putInt 0 -- no children

    let (_, V3 x y z) = bounds model

    putInt . succ . fromIntegral $ x
    putInt . succ . fromIntegral $ y
    putInt . succ . fromIntegral $ z

    putByteString "XYZI"
    putInt . size $ model
      -- ^ Here I'm saying how many bytes there are in the XYZI section
    putInt 0 -- no children

    let voxels = toVoxels model

    putInt (length voxels)
      -- ^ While here I'm saying how many *voxels* there are.

    forM_ voxels $ \(V3 x' y' z', c)  -> putWord8 x'
                                      >> putWord8 y'
                                      >> putWord8 z'
                                      >> putWord8 c

  case palette of
    DefaultPalette -> pure ()
    CustomPalette colors -> do
      putByteString "RGBA"
      forM_ colors $ \(Color (r, g, b, a)) -> putWord8 r
                                           >> putWord8 g
                                           >> putWord8 b
                                           >> putWord8 a

putInt :: Int -> Put
putInt = putWord32le . fromIntegral
