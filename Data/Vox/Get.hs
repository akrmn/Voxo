{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections  #-}

module Data.Vox.Get where

import           Control.Monad        (replicateM, void, when)
import           Data.Array           (listArray)
import           Data.Binary.Get
import qualified Data.ByteString.Lazy as BL
import           Data.Char            (chr)
import           Data.Semigroup       (Semigroup (..))
import qualified Data.Sequence        as Seq (replicateM)
import           Data.Vox.Palette     (Color (..), Palette (..))
import           Data.Vox.VoxData
import           Data.Vox.Model
import           Data.Word            (Word8)

parseVoxFileStream :: (Model model)
  => BL.ByteString
  -> Either (ByteOffset, String) (Vox model)
parseVoxFileStream input =
   case runGetOrFail getVox input of
      Left (_, offset, err) -> Left (offset, err)
      Right (_, _, result)  -> Right result


getVox :: Model model => Get (Vox model)
getVox = do
  ident <- getIdentifier
  when (ident /= "VOX ") . fail $
    "VOX file must start with chunk id 'VOX ', instead of '" <> ident <> "'."

  _versionNumber <- getInt

  ident' <- getIdentifier
  when (ident' /= "MAIN") . fail $
    "Expected MAIN chunk instead of " <> ident' <> "."

  contentsBytes <- getInt
  childrenBytes <- getInt

  when (contentsBytes /= 0) $ fail "MAIN chunk cannot have contents."

  (models, palette) <- isolate childrenBytes $ do
    numModels <- getPackChunk
    models <- Seq.replicateM numModels getModel
    palette <- getPalette
    void getMaterials
    pure (models, palette)

  pure Vox { models, palette }


getPackChunk :: Get Int
getPackChunk = do
  ident <- lookAhead getIdentifier
  case ident of
    "PACK" -> do
      void getIdentifier
      contentsBytes <- getInt
      childrenBytes <- getInt

      numModels <- isolate contentsBytes getInt

      when (childrenBytes /= 0) $ fail "PACK chunk cannot have children."

      pure numModels

    _ -> pure 1


getModel :: Model model => Get (model Point Word8)
getModel = do
  V3 x y z <- getSize
  ident <- getIdentifier
  when (ident /= "XYZI") . fail $
    "Expected XYZI chunk instead of " <> ident <> "."

  contentsBytes <- getInt
  childrenBytes <- getInt

  (_numVoxels, voxels) <- isolate contentsBytes $ do
    numVoxels <- getInt
    voxels <- replicateM numVoxels getVoxel
    pure (numVoxels, voxels)

  when (childrenBytes /= 0) $ fail "XYZI chunk cannot have children."

  pure $ fromVoxels (V3 0 0 0, V3 (x-1) (y-1) (z-1)) voxels


getSize :: Get Point
getSize = do
  ident <- getIdentifier
  when (ident /= "SIZE") . fail $
    "Expected SIZE chunk instead of " <> ident <> "."

  contentsBytes <- getInt
  childrenBytes <- getInt

  dims <- isolate contentsBytes $
    V3 <$> getInt' <*> getInt' <*> getInt'

  when (childrenBytes /= 0) $ fail "SIZE chunk cannot have children."

  pure dims

  where
    getInt' = aux =<< getInt
    aux x
      | x < 0     = fail "Negative dimension in model"
      | x > 255   = fail "Too large dimension in model"
      | otherwise = pure (fromIntegral x)


getVoxel :: Get Voxel
getVoxel = aux <$> getWord8 <*> getWord8 <*> getWord8 <*> getWord8
  where aux x y z w = (V3 x y z, w)


getPalette :: Get Palette
getPalette = do
  empty <- isEmpty
  if empty
    then pure DefaultPalette
    else lookAhead getIdentifier >>= \case
      "RGBA" -> do
        void getIdentifier
        contentsBytes <- getInt
        childrenBytes <- getInt

        colors <- isolate contentsBytes . replicateM 256 $
          fmap Color $ (,,,) <$> getWord8 <*> getWord8 <*> getWord8 <*> getWord8

        when (childrenBytes /= 0) $ fail "RGBA chunk cannot have children."

        pure . CustomPalette $ listArray (0, 255) colors

      "MATT" -> pure DefaultPalette
      ident -> fail $
        "Expected RGBA or MATT chunk instead of " <> ident <> "."


getMaterials :: Get ()
getMaterials = do
  empty <- isEmpty
  if empty
    then pure ()
    else lookAhead getIdentifier >>= \case
      "MATT" -> do
        void getIdentifier
        contentsBytes <- getInt
        childrenBytes <- getInt

        skip contentsBytes

        when (childrenBytes /= 0) $ fail "MATT chunk cannot have children."

        getMaterials

      ident -> fail $
        "Expected MATT chunk instead of " <> ident <> "."


getInt :: Get Int
getInt = fromIntegral <$> getWord32le


getNWords :: Int -> Get [Word8]
getNWords n = replicateM n getWord8


getNChars :: Int -> Get String
getNChars = fmap (fmap byteToChar) . getNWords
  where
    byteToChar :: Word8 -> Char
    byteToChar = chr . fromIntegral


getIdentifier :: Get String
getIdentifier = getNChars 4
