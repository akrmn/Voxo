{-# LANGUAGE OverloadedLists #-}

module Test where

import Language.Voxo
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq (fromList)
import Data.Word (Word8)
import Prelude hiding (Either (..))


simple :: VoxoCommand
simple = Sequence
  [ Debug On
  , Mode Move
  , TracePos
  , Go Up 20
  , TracePos
  , Mode Fill
  , Go Down 10 
  , TracePos
  , SaveTo "out.vox" ]

eightCubes :: VoxoCommand
eightCubes = Sequence
  [ fourCubes 0
  , Go Up 11
  , fourCubes 40
  , Frames On
  , SaveTo "out4.vox" ]

fourCubes :: Word8 -> VoxoCommand
fourCubes n = Sequence
  [ aCube (n + 0)
  , store'
  , Mode Move, Go Forward 11 
  , aCube (n + 13)
  , store'
  , Mode Move, Go Right 11
  , aCube (n + 26)
  , store'
  , Mode Move, Go Back 11
  , aCube (n + 39)
  , store'
  , Mode Move, Go Left 11
  ]

store' :: VoxoCommand
store' = Sequence
  [ Frames On, StoreFrame, Frames Off ]

aCube :: Word8 -> VoxoCommand
aCube n = Sequence
  [ Debug Off
  , Frames Off
  , Mode Fill
  , SetColor (n +1)
  , Go Forward 10
  , StoreFrame

  , Turn Right
  , SetColor (n +2)
  , Go Forward 10
  , StoreFrame

  , Turn Right
  , SetColor (n +3)
  , Go Forward 10
  , StoreFrame

  , Turn Right
  , SetColor (n +4)
  , Go Forward 10
  , StoreFrame

  , Turn Right
  , SetColor (n +5)
  , Go Up 10
  , StoreFrame

  , SetColor (n +6)
  , Go Forward 10
  , StoreFrame

  , Turn Right
  , SetColor (n +7)
  , Go Forward 10
  , StoreFrame

  , Turn Right
  , SetColor (n +8)
  , Go Forward 10
  , StoreFrame

  , Turn Right
  , SetColor (n +9)
  , Mode Draw
  , Go Forward 10
  , Mode Fill
  , StoreFrame

  , Turn Right
  , SetColor (n +10)
  , Mode Move
  , Go Forward 10
  , Mode Fill
  , Go Down 10
  , StoreFrame

  , Turn Right
  , SetColor (n +11)
  , Mode Move
  , Go Forward 10
  , Mode Fill
  , Go Up 10
  , StoreFrame

  , Turn Right
  , SetColor (n +12)
  , Mode Move
  , Go Forward 10
  , Mode Fill
  , Go Down 10
  , StoreFrame

  , Mode Move
  , Turn Right
  , Go Forward 10
  , Turn Right
  ]
