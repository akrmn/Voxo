{-# LANGUAGE OverloadedLists #-}

module Test where

import Language.Voxo
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq (fromList)
import Data.Word (Word8)
import Prelude hiding (Either (..))

myVoxo :: VoxoCommand
myVoxo = Sequence
  [ layer
  , Mode Move

  , layer
  , SaveTo "out.vox" ]

layer :: VoxoCommand
layer = Sequence
  [ squaryyy 10
  , Mode Move
  , Go Up 20

  , Turn Left
  , Turn Up
  , Turn Up

  , squaryyy 10 
  , Turn Down
  , Turn Down
  , Turn Right
  ]

squaryyy :: Word8 -> VoxoCommand
squaryyy n = let m = fromIntegral n in Sequence
  [ squaryy n
  , Mode Move
  , Go Forward (4 * m)
  , Turn Right

  , squaryy n
  , Mode Move
  , Go Forward (4 * m)
  , Turn Right

  , squaryy n
  , Mode Move
  , Go Forward (4 * m)
  , Turn Right

  , squaryy n
  , Mode Move
  , Go Forward (4 * m)
  , Turn Right
  ]

squaryy :: Word8 -> VoxoCommand
squaryy n = let m = fromIntegral n in Sequence
  [ squary n
  , Mode Move
  , Go Forward (2 * m)
  , Turn Right

  , squary n
  , Mode Move
  , Go Forward (2 * m)
  , Turn Right

  , squary n
  , Mode Move
  , Go Forward (2 * m)
  , Turn Right

  , squary n
  , Mode Move
  , Go Forward (2 * m)
  , Turn Right
  ]

squary :: Word8 -> VoxoCommand
squary n = Sequence
  [ Mode Fill
  , squary' n

  , Mode Move
  , Go Down (fromIntegral n)
  , Mode Fill
  ]

  where
    squary' :: Word8 -> VoxoCommand
    squary' 0 = Nop
    squary' n = Sequence [square n, Go Up 1, squary' (n-1)]

    square n = let m = fromIntegral n in Sequence
      [ SetColor n
      , Go Forward m
      , Turn Right
      , SetColor (n*2)
      , Go Forward m
      , Turn Right
      , SetColor (n*3)
      , Go Forward m
      , Turn Right
      , SetColor (n*4)
      , Go Forward m
      , Turn Right 
      ]
