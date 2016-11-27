{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Data.Vox.Put where

import           Control.Monad             (forM_, replicateM, replicateM_,
                                            void, when)
import           Data.Array                (listArray)
import           Data.Array.IArray         (assocs, bounds)
import           Data.Binary.Put
import qualified Data.ByteString.Lazy      as BL
import           Data.Char                 (chr)
import           Data.Semigroup            (Semigroup (..))
import           Data.Sequence             (Seq)
import qualified Data.Sequence             as Seq (empty, replicateM)
import           Data.Vox.Internal.Sizable (Sizable (..))
import           Data.Vox.Palette          (Color (..), Palette (..))
import           Data.Vox.VoxData          (Model (..), Vox (..), Voxel)
import           Data.Word                 (Word8)

putVox :: Vox -> Put
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

  forM_ models $ \m@(Model vxls) -> do
    putByteString "SIZE"
    putInt 12
    putInt 0

    let (_, (x, y, z)) = bounds vxls

    putInt . succ . fromIntegral $ x
    putInt . succ . fromIntegral $ y
    putInt . succ . fromIntegral $ z

    putByteString "XYZI"
    putInt . size $ m
    putInt 0

    let voxels = filter ((/= 0) . snd) (assocs vxls)

    putInt (length voxels)

    forM_ voxels $ \((x', y', z'), c) -> putWord8 x'
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
