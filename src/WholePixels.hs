module WholePixels
  ( mainWithPainting
  , Generate
  , Seed (..)
  , getSize
  , getGridSpec
  , Painting (..)
  , cairo
  , module Relude
  , module Graphics.Rendering.Cairo
  , module WholePixels.Random
  , module WholePixels.Color
  , module WholePixels.Target
  , module WholePixels.Geometry
  , module Data.Bool
  , module Data.Maybe
  , module Control.Monad.Random.Strict
  , module GHC.Float
  , setSourceHSV
  , setSourceHSVA
  , hsva
  , fillScreenHSV
  , patternAddColorStopHSV
  , patternAddColorStopHSVA
  , rotateToDir
  , strokeArc
  , fillArc
  , fillCircle
  , fillOriginCircle
  , fillRect
  , strokeRect
  , zoomingToRect
  )
where

import Control.Monad.Random.Strict hiding (fail, fromList)
import Data.Bool
import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSV
import Data.Maybe
import qualified Data.Random as D
import Data.Random.Internal.Source
import Data.Time.Clock.POSIX
import GHC.Float (atan2)
import Graphics.Rendering.Cairo
import Relude
import System.Directory
import System.IO.Temp
import WholePixels.Color
import WholePixels.Geometry
import WholePixels.Random
import WholePixels.Target

data Seed = RandomSeed | FixedSeed !Int

data Painting
  = Painting
      { paintingName :: !String
      , paintingVersion :: !String
      , paintingGenerate :: Generate ()
      , paintingBackgroundColor :: Maybe HSV
      , paintingSeed :: Seed
      }

mainWithPainting :: Painting -> Target -> IO ()
mainWithPainting Painting {..} target = do
  seed <-
    case paintingSeed of
      RandomSeed -> round . (* 1000) <$> getPOSIXTime
      FixedSeed s -> pure s
  print ("Seed", seed)
  let stdGen = mkStdGen seed
      (w, h) = dimensions target
  createDirectoryIfMissing False $ "./images/"
  createDirectoryIfMissing False $ "./images/" <> paintingName
  createDirectoryIfMissing False $ "./images/" <> paintingName <> "/progress"
  let ctx = GenerateCtx w h (gridSpec target)
  surface <- createImageSurface FormatARGB32 w h
  void . renderWith surface . flip runReaderT ctx . flip runRandT stdGen $ do
    paintingGenerate
  putStrLn "Generating art..."
  let filename =
        concat
          [ "images/"
          , paintingName
          , "/"
          , paintingVersion
          , "-"
          , show seed
          , "-"
          , show target
          , ".png"
          ]
      latest = "images/latest.png"
  putStrLn $ "Writing " <> filename
  putStrLn $ "Writing " <> latest
  withSystemTempFile "wholepixels" $ \tmp _handle -> do
    surfaceWriteToPNG surface tmp
    renameFile tmp filename
  withSystemTempFile "wholepixels" $ \tmp _handle -> do
    surfaceWriteToPNG surface tmp
    renameFile tmp latest

data GenerateCtx
  = GenerateCtx
      { gcWidth :: Int
      , gcHeight :: Int
      , gcGridSpec :: GridSpec
      }

type Generate a = RandT StdGen (ReaderT GenerateCtx Render) a

instance Monad m => D.MonadRandom (RandT StdGen (ReaderT GenerateCtx m)) where

  -- |Generate a uniformly distributed random 'Word8'
  getRandomWord8 = getRandom

  -- |Generate a uniformly distributed random 'Word16'
  getRandomWord16 = getRandom

  -- |Generate a uniformly distributed random 'Word32'
  getRandomWord32 = getRandom

  -- |Generate a uniformly distributed random 'Word64'
  getRandomWord64 = getRandom

  -- |Generate a uniformly distributed random 'Double' in the range 0 <= U < 1
  getRandomDouble = getRandom

  -- |Generate a uniformly distributed random 'Integer' in the range 0 <= U < 256^n
  getRandomNByteInteger n = getRandomR (0, 256 ^ n)

getSize :: Num a => Generate (a, a)
getSize = do
  (w, h) <- asks (\ctx -> (gcWidth ctx, gcHeight ctx))
  pure (fromIntegral w, fromIntegral h)

getGridSpec :: Generate GridSpec
getGridSpec = asks gcGridSpec

-- | Lift a Cairo action into a Generate action
cairo :: Render a -> Generate a
cairo = lift . lift

setSourceHSV :: HSV -> Render ()
setSourceHSV color = setSourceHSVA (color `WithAlpha` 1)

setSourceHSVA :: WithAlpha HSV -> Render ()
setSourceHSVA (WithAlpha HSV {..} alpha) =
  hsva hsvHue hsvSaturation hsvValue alpha

hsva :: Double -> Double -> Double -> Double -> Render ()
hsva h s v = setSourceRGBA channelRed channelGreen channelBlue
  where
    RGB {..} = hsv h s v

fillScreenHSV :: HSV -> Generate ()
fillScreenHSV color = do
  (w, h) <- getSize
  cairo $ do
    rectangle 0 0 w h
    setSourceHSV color *> fill

patternAddColorStopHSV :: Pattern -> Double -> HSV -> Render ()
patternAddColorStopHSV pat offset (HSV h s v) =
  let RGB r g b = hsv h s v
  in patternAddColorStopRGBA pat offset r g b 1

patternAddColorStopHSVA :: Pattern -> Double -> WithAlpha HSV -> Render ()
patternAddColorStopHSVA pat offset (WithAlpha (HSV h s v) a) =
  let RGB r g b = hsv h s v
  in patternAddColorStopRGBA pat offset r g b a

rotateToDir :: Direction -> Render ()
rotateToDir R = pure ()
rotateToDir D = rotate (pi / 2)
rotateToDir L = rotate pi
rotateToDir U = rotate (3 * pi / 2)

strokeArc :: Double -> Double -> Double -> Double -> Double -> Render ()
strokeArc x y r a0 a1 = do
  newPath
  arc x y r a0 a1
  stroke

fillArc :: Double -> Double -> Double -> Double -> Double -> Render ()
fillArc x y r a0 a1 = do
  newPath
  arc x y r a0 a1
  fill

fillCircle :: Double -> Double -> Double -> Render ()
fillCircle x y r = fillArc x y r 0 (2 * pi)

fillOriginCircle :: Double -> Render ()
fillOriginCircle r = fillArc 0 0 r 0 (2 * pi)

fillRect :: Rect -> Render ()
fillRect Rect {..} = do
  newPath
  moveTo rx ry
  lineTo (rx + rw) ry
  lineTo (rx + rw) (ry + rh)
  lineTo rx (ry + rh)
  fill

strokeRect :: Rect -> Render ()
strokeRect Rect {..} = do
  newPath
  moveTo rx ry
  lineTo (rx + rw) ry
  lineTo (rx + rw) (ry + rh)
  lineTo rx (ry + rh)
  stroke

zoomingToRect :: Rect -> Render () -> Render ()
zoomingToRect Rect {..} a = do
  save
  moveTo rx ry
  lineTo (rx + rw) ry
  lineTo (rx + rw) (ry + rh)
  lineTo rx (ry + rh)
  clip
  translate (rx + rw / 2) (ry + rh / 2)
  scale (rw / 2) (rh / 2)
  newPath
  a
  restore
