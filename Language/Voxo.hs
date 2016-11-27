{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Voxo
  ( VoxoCommand (..)
  , Point
  , Direction (..)
  , Axis (..)
  , Mode (..)
  , Switch (..)
  , runVoxo ) where

import           Control.Lens              (both, each, makeLenses, use, (%=),
                                            (%~), (&), (.=), (.~), (<&>))
import           Control.Monad             (forM_, when)
import           Control.Monad.Trans.State (StateT (..), evalStateT)
import           Data.Array                (Array, listArray, (!))
import           Data.Array.IO             (IOArray)
import           Data.Array.MArray         (freeze, getBounds, newArray,
                                            readArray, writeArray)
import           Data.Binary.Put           (runPut)
import           Data.Bool                 (bool)
import qualified Data.ByteString.Lazy      as BL
import           Data.Ix                   (Ix, range)
import           Data.Maybe                (fromJust)
import           Data.Semigroup            (Semigroup (..))
import           Data.Sequence             (Seq, (|>))
import qualified Data.Sequence             as Seq (empty, singleton)
import           Data.Vox
import           Data.Word                 (Word8)
import           MonadUtils                (liftIO)
import           Prelude                   hiding (Either (..))
import           System.IO                 (IOMode (WriteMode), openFile)

type Distance = Int

data VoxoCommand
  = Mode Mode
  | Go Direction Distance
  | GoTo Point
  | SetOrientation Orientation
  | Turn Direction
  | Cross Axis
  | SetColor Word8
  | Zero
  | Nop
  | StoreFrame
  | Frames Switch
  | Debug Switch
  | SaveTo FilePath
  | Sequence (Seq VoxoCommand)
  | Trace String
  | TracePos
  deriving (Show)

data Switch = Off | On deriving (Eq, Show)

toBool :: Switch -> Bool
toBool = \case
  On  -> True
  Off -> False

data Direction
  = Up
  | Down
  | Left
  | Right
  | Forward
  | Back
  deriving (Eq, Show)

data Mode
  = Draw
  | Fill
  | Move
  | Erase
  | Paint
  deriving (Show)

data Axis
  = X  | Y  | Z
  | X' | Y' | Z'
  deriving (Eq, Ord, Enum, Show, Ix)

opTable :: Array (Axis, Axis) Axis
opTable = listArray ((X, X), (Z', Z'))
  {-  <>   X   Y   Z     X'  Y'  Z'-}
  {-X -} [ X , Z , Y',   X , Z', Y
  {-Y -} , Z', Y , X ,   Z , Y , X'
  {-Z -} , Y , X', Z ,   Y', X , Z

  {-X'-} , X', Z', Y ,   X', Z , Y'
  {-Y'-} , Z , Y', X',   Z', Y', X
  {-Z'-} , Y', X , Z',   Y , X', Z' ]

instance Semigroup Axis where
  a <> b = opTable ! (a, b)

type Orientation = (Axis, Axis)

data VoxoState = VoxoState
  { _pos         :: Point
  , _orientation :: Orientation
  , _mode        :: Mode
  , _color       :: Word8
  , _stpalette   :: Palette
  , _frames      :: Seq Model
  , _framesSw    :: Switch
  , _debugSw     :: Switch
  , _voxels      :: IOArray Point Word8 }

makeLenses ''VoxoState

initialVoxoState :: VoxoState
initialVoxoState = VoxoState
  { _pos         = (0, 0, 0)
  , _orientation = (Z, X)
  , _mode        = Fill
  , _color       = 1
  , _stpalette   = DefaultPalette
  , _frames      = Seq.empty
  , _framesSw    = Off
  , _debugSw     = Off
  , _voxels      = undefined }


move :: Point -> Axis -> Distance -> StateT VoxoState IO (Point, Point)
move p a d = do
  bounds <- liftIO . getBounds =<< use voxels
  pure . (both %~ bound bounds) $ case a of
    X  -> ((x + d', y, z), (x + d, y, z))
    Y  -> ((x, y + d', z), (x, y + d, z))
    Z  -> ((x, y, z + d'), (x, y, z + d))
    X' -> ((x - d', y, z), (x - d, y, z))
    Y' -> ((x, y - d', z), (x, y - d, z))
    Z' -> ((x, y, z - d'), (x, y, z - d))

  where
    (x :: Int, y, z) = p & each %~ fromIntegral
    d' | d == 0 =  0
       | d >  0 =  1
       | d <  0 = -1

    bound :: (Point, Point)
          -> (Int, Int, Int)
          -> Point
    bound (low, high) (x, y, z) =
      let (lx, ly, lz) = low  & each %~ fromIntegral
          (hx, hy, hz) = high & each %~ fromIntegral
      in  ( (x `max` lx) `min` hx
          , (y `max` ly) `min` hy
          , (z `max` lz) `min` hz
          ) & each %~ fromIntegral


runVoxo :: VoxoCommand -> IO ()
runVoxo c = do
  vxls <- newArray ((0, 0, 0), (44, 44, 44)) 0
  evalStateT (runVoxo' c) (initialVoxoState & voxels .~ vxls)


runVoxo' :: VoxoCommand -> StateT VoxoState IO ()
runVoxo' = \case
  Mode m -> mode .= m

  Go Forward 0 -> pure ()

  Go Forward units -> do
    (u,r) <- use orientation
    arr <- use voxels
    m <- use mode
    p <- use pos
    c <- use color
    (p', q) <- move p (u <> r) units


    let vxls = if p == q
          then []
          else range (p' `min` q, p' `max` q)

    use debugSw >>= flip when (liftIO $ print (p, (u<>r), units, p', q, vxls)) . toBool

    case m of
      Draw  -> liftIO . forM_ vxls $ \vxl -> writeArray arr vxl c
      Erase -> liftIO . forM_ vxls $ \vxl -> writeArray arr vxl 0
      Fill  -> liftIO . forM_ vxls $ \vxl ->
        (readArray arr vxl <&> (== 0)) >>=
          flip when (writeArray arr vxl c)
      Paint -> liftIO . forM_ vxls $ \vxl ->
        (readArray arr vxl <&> (/= 0)) >>=
          flip when (writeArray arr vxl c)
      Move  -> pure ()

    pos .= q

  Go Up n -> runVoxo' $ Sequence
    [ Turn Up
    , Go Forward n
    , Turn Down ]

  Go Right n -> runVoxo' $ Sequence
    [ Turn Right
    , Go Forward n
    , Turn Left ]

  Go Back n -> runVoxo' $ Go Forward (-n)
  Go Down n -> runVoxo' $ Go Up      (-n)
  Go Left n -> runVoxo' $ Go Right   (-n)

  GoTo point -> pos .= point

  SetOrientation o -> orientation .= o

  Turn d -> orientation %= turn d
    where
      turn d o@(up, right) = case d of
        Up      -> (right <> up, right)
        Down    -> (up <> right, right)

        Right   -> (up, right <> up)
        Left    -> (up, up <> right)

        Forward -> o
        Back    -> turn Right . turn Right $ o

  Cross axis -> orientation . both %= (<> axis)

  SetColor 0 -> liftIO $ putStrLn "Can't set color to 0"

  SetColor c -> color .= c

  Zero -> do
    arr <- use voxels
    bounds <- liftIO . getBounds =<< use voxels
    liftIO . forM_ (range bounds) $ \vxl -> writeArray arr vxl 0

  Nop -> pure ()

  Frames sw -> framesSw .= sw

  Debug sw -> debugSw .= sw

  StoreFrame -> do
    switch <- use framesSw
    when (switch == On) $ do
      model <- fmap Model $ liftIO . freeze =<< use voxels
      frames %= (|> model)

  SaveTo path -> do
    models <- use framesSw >>= \case
      On  -> use frames
      Off -> fmap (Seq.singleton . Model) $ liftIO . freeze =<< use voxels
    palette <- use stpalette

    h <- liftIO $ openFile path WriteMode
    liftIO $ BL.hPutStr h . runPut . putVox $ Vox
      { models
      , palette }

  Sequence commands -> mapM_ runVoxo' commands

  Trace msg -> liftIO $ putStrLn msg

  TracePos -> (,) <$> use pos <*> use orientation >>= liftIO . print
