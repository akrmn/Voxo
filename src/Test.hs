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

aCube :: VoxoCommand
aCube = Sequence
  [ Frames On
  , SetColor 4
  , Go Forward 20
  , StoreFrame

  , Turn Right
  , SetColor 8
  , Go Forward 20
  , StoreFrame

  , Turn Right
  , SetColor 12
  , Go Forward 20
  , StoreFrame

  , Turn Right
  , SetColor 16
  , Go Forward 20
  , StoreFrame

  , Turn Right
  , SetColor 20
  , Go Up 20
  , StoreFrame

  , SetColor 24
  , Go Forward 20
  , StoreFrame

  , Turn Right
  , SetColor 28
  , Go Forward 20
  , StoreFrame

  , Turn Right
  , SetColor 32
  , Go Forward 20
  , StoreFrame

  , Turn Right
  , SetColor 36
  , Go Forward 20
  , StoreFrame

  , Turn Right
  , SetColor 40
  , Mode Move
  , Go Forward 20
  , Mode Fill
  , Go Down 20
  , StoreFrame

  , Turn Right
  , SetColor 44
  , Mode Move
  , Go Forward 20
  , Mode Fill
  , Go Up 20
  , StoreFrame

  , Turn Right
  , SetColor 48
  , Mode Move
  , Go Forward 20
  , Mode Fill
  , Go Down 20
  , StoreFrame
  , SaveTo "out.vox"
  ]

-- aSquare :: VoxoCommand
-- aSquare = Sequence
--   [ Forward 20
--   , StoreFrame
--   , SetColor 5
--   , Turn Z'
--   , Forward 20
--   , StoreFrame
--   , SetColor 10
--   , Turn Z'
--   , Forward 20
--   , StoreFrame
--   , SetColor 15
--   , Turn Z'
--   , Forward 20
--   , StoreFrame
--   , SaveTo "out.vox"
--   ]

-- myVoxo :: VoxoCommand
-- myVoxo = Sequence
--   [ Frames Off
--   , squaryy 10
--   , SaveTo "out.vox" ]

-- squaryy :: Word8 -> VoxoCommand
-- squaryy n = Sequence
--   [ 
--     -- squary n
--     Mode Move
--   , Forward n

--   -- , Frames On
--   -- , StoreFrame
--   -- , Frames Off

--   -- , squary n
--   -- , Mode Move
--   , Turn Z'
--   , Forward n

--   -- , Frames On
--   -- , StoreFrame
--   -- , Frames Off

--   , Frames On

--   , squary n
--   , Mode Move
--   , Turn Z'
--   , Forward n

--   -- , Frames On
--   -- , StoreFrame
--   -- , Frames Off

--   -- , squary n
--   -- , Mode Move
--   -- , Turn Z'
--   -- , Forward n

--   , Frames On
--   , StoreFrame

--   ]

-- squary :: Word8 -> VoxoCommand
-- squary n = Sequence
--   [ Mode Fill
--   , squary' n
--   , StoreFrame
--   , Turn X'
--   , Mode Move
--   , Forward n
--   , Mode Fill
--   , Turn X
--   , StoreFrame 
--   ]

--   where
--     squary' :: Word8 -> VoxoCommand
--     squary' 0 = Nop
--     squary' n = Sequence [square n, StoreFrame, oneUp, squary' (n-1)]

--     square n = Sequence
--       [ SetColor n
--       , Forward n
--       , Turn Z'
--       , SetColor (n*2)
--       , Forward n
--       , Turn Z'
--       , SetColor (n*3)
--       , Forward n
--       , Turn Z'
--       , SetColor (n*4)
--       , Forward n
--       , Turn Z' 
--       ]

--     oneUp = Sequence
--       [ Turn X
--       , Mode Move
--       , Forward 1
--       , Mode Fill
--       , Turn X'
--       ]

