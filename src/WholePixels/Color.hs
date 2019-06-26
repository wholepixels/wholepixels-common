module WholePixels.Color where

import Relude

data HSV
  = HSV
      { hsvHue :: Double
      , hsvSaturation :: Double
      , hsvValue :: Double
      }
  deriving (Show, Read, Eq, Ord)

data WithAlpha color
  = WithAlpha
      { waColor :: color
      , waAlpha :: Double
      }
  deriving (Show, Read, Eq, Ord)

white :: HSV
white = HSV 0 0 1

black :: HSV
black = HSV 0 0 0

red :: HSV
red = HSV 0 1 1

mediumRed :: HSV
mediumRed = HSV 0 1 0.57

darkRed :: HSV
darkRed = HSV 0 1 0.27

gold :: HSV
gold = HSV 51 1 0.5

yellow :: HSV
yellow = HSV 50 1 1

brightGreen :: HSV
brightGreen = HSV 120 1 1

green :: HSV
green = HSV 120 0.8 1

dullGreen :: HSV
dullGreen = HSV 120 1 0.5

darkBlue :: HSV
darkBlue = HSV 219 0.77 0.43

blue :: HSV
blue = HSV 220 1 1

lightBlue :: HSV
lightBlue = HSV 220 0.3 1

teal :: HSV
teal = HSV 175 0.31 0.43

sand :: HSV
sand = HSV 46 0.39 0.92

avocadoPeel :: HSV
avocadoPeel = HSV 81 0.98 0.31

avocadoInnerGreen :: HSV
avocadoInnerGreen = HSV 70 0.88 0.61

avocadoInnerYellowishGreen :: HSV
avocadoInnerYellowishGreen = HSV 60 0.98 0.71

avocadoPitBrown :: HSV
avocadoPitBrown = HSV 20 0.4 0.3

avocadoPitHighlightBrown :: HSV
avocadoPitHighlightBrown = HSV 50 0.3 0.6

avocadoPitSlotGreen :: HSV
avocadoPitSlotGreen = HSV 60 0.98 0.5

avocadoPitSlotShadowGreen :: HSV
avocadoPitSlotShadowGreen = HSV 60 0.98 0.3

purple :: HSV
purple = HSV 271 0.79 0.68

pink :: HSV
pink = HSV 300 0.97 0.69

gray :: HSV
gray = HSV 0 0 0.7

lightGray :: HSV
lightGray = HSV 0 0 0.8

darkGray :: HSV
darkGray = HSV 0 0 0.4

brown :: HSV
brown = HSV 20 0.4 0.2

orange :: HSV
orange = HSV 17 0.89 0.90

fixHue :: Double -> Double
fixHue h = h - 360 * fromIntegral (floor @Double @Int h `div` 360)

complementary :: HSV -> HSV
complementary (HSV h s v) = HSV (fixHue $ h + 180) s v

splitComplementary :: HSV -> (HSV, HSV)
splitComplementary (HSV h s v) =
  (HSV (fixHue $ h + 150) s v, HSV (fixHue $ h + 210) s v)

splitTriangle :: HSV -> (HSV, HSV)
splitTriangle (HSV h s v) =
  (HSV (fixHue $ h + 120) s v, HSV (fixHue $ h + 240) s v)

analogous :: HSV -> [HSV]
analogous (HSV h s v) =
  map
    (\dh -> HSV (fixHue $ h + dh) s v)
    [0, 30.. 360]

data Palette
  = Palette
      { baseColor :: !HSV
      , colors :: ![HSV]
      }
  deriving (Show, Eq)
