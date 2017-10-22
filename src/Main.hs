{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Sequence        as Seq (singleton)

import Control.Applicative   (Applicative (..), ZipList (..))
import Control.Arrow
import Data.Binary.Put
import Data.Functor.Foldable
import Data.Ix
import Data.List
import Data.Monoid
import Data.Vox              hiding (toVoxels)
import Data.Vox.Model        (BoundedMap (..))
import Data.Vox.Put
import Linear.V3
import System.IO
import Data.Functor.Classes
import GHC.Generics

data OctreeF w o
  = Leaf w
  | Branch
      -- BLT = back, left, top
      -- blt = not back, not left, not top
      { _bLT :: o, _blT :: o
      , _bLt :: o, _blt :: o
                            , _BLT :: o, _BlT :: o
                            , _BLt :: o, _Blt :: o }
  deriving (Functor, Show, Generic)

type Octree w = Fix (OctreeF w)

type OctreeW8 = Octree Word8

main :: IO ()
main = main

save :: FilePath -> OctreeW8 -> IO ()
save path octree = do
  withFile (path <> ".vox") WriteMode $ \h ->
    BL.hPutStr h . runPut . putVox $ Vox
    { palette = DefaultPalette
    , models  = Seq.singleton
              $ fromVoxels @BoundedMap (0, 2 ^ dim - 1) $ vxls }
  where
    dim  = getDimension octree
    vxls = toVoxels     octree


leaf' :: w -> Octree w
leaf' w = Fix (Leaf w)

leaf :: Word8 -> OctreeW8
leaf = leaf'


branch' :: ()
  => Octree w -> Octree w
  -> Octree w -> Octree w
                      -> Octree w -> Octree w
                      -> Octree w -> Octree w
  -> Octree w
branch' a b c d e f g h =
    Fix (Branch a b c d e f g h)

branch :: ()
  => OctreeW8 -> OctreeW8
  -> OctreeW8 -> OctreeW8
                      -> OctreeW8 -> OctreeW8
                      -> OctreeW8 -> OctreeW8
  -> OctreeW8
branch = branch'

toVoxels :: OctreeW8 -> [Voxel]
toVoxels = zygo dimensionCata toVoxelsZygo

toVoxelsZygo :: OctreeF Word8 (Int, [Voxel]) -> [Voxel]
toVoxelsZygo (Leaf c) = [(0, c)]
toVoxelsZygo (Branch {..}) =
    concat . getZipList $ adjust
        <$> ZipList branches
        <*> ZipList (range (0, 1))
    where
        branches = [_blt, _blT, _bLt, _bLT, _Blt, _BlT, _BLt, _BLT ]
        maxDim = maximum $ fst <$> branches
        targetDim = succ maxDim
        adjust (dim, voxs) baseOffset =
            [ (baseOffset * 2 ^ maxDim + extraOffset + xyz * 2 ^ (maxDim - dim), c)
            | (xyz, c) <- voxs, extraOffset <- range (0, 2 ^ (maxDim - dim) - 1)]

getDimension :: OctreeW8 -> Int
getDimension = cata dimensionCata

dimensionCata :: OctreeF w Int -> Int
dimensionCata (Leaf _) = 0
dimensionCata (Branch aaa aab aba abb baa bab bba bbb) =
  succ $ maximum [aaa,aab,aba,abb,baa,bab,bba,bbb]

builderApo :: (Int, Word8) -> OctreeF Word8 (Either OctreeW8 (Int, Word8))
builderApo (0, c) = Leaf c
builderApo (n, c) = Branch
  -- (Left . leaf $ 0) (Left . leaf $ 0)
  -- (Left . leaf $ 0) (Right (n-1, c+1))
  --   (Right (n-1, c+2)) (Right (n-1, c+3))
  --   (Right (n-1, c+3)) (Right (n-1, c+4))
  (Left . leaf $ 0)  (Right (n-1, c+2))
  (Right (n-1, c+4)) (Right (n-1, c+3))
    (Right (n-1, c+2)) (Right (n-1, c+3))
    (Right (n-1, c+3)) (Right (n-1, c+4))


data ABC = A | B | C

myApo :: (Int, ABC, Word8) -> OctreeF Word8 (Either OctreeW8 (Int, ABC, Word8))
myApo (0, _, c) = Leaf c
myApo (n, x, c) = case x of
    A -> Branch
        (blank     ) (r $ c +! 1 )
        (r $ c +! 5) (r $ c +! 10)
            (r $ c +! 10) (r $ c +! 5)
            (r $ c +! 1 ) (blank     )

    B -> Branch
        (blank) (blank     )
        (blank) (r $ c +! 1)
            (blank     ) (r $ c +! 1)
            (r $ c +! 1) (r $ c +! 2)

    C -> Branch
        (r $ c +! 4) (blank)
        (blank     ) (r $ c +! 4)
            (blank      ) (r $ c +! 20)
            (r $ c +! 20) (blank      )

    where
        r c = Right (n-1, next x, c)
        blank = Left $ leaf 0
        infixl 6 +!
        a +! b =
            let ab = a + b
            in if ab == 0 then 1 else ab
        next x = case x of
            A -> B
            B -> C
            C -> A
