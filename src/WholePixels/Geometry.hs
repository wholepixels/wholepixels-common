module WholePixels.Geometry where

import Relude

fitGrid :: (Double, Double) -> Int -> ((Int, Int), Double)
fitGrid (canvas_w, canvas_h) desired_size =
  ((bool swap id (canvas_w > canvas_h) (grid_min, grid_max)), cell_size)
  where
    canvas_max = max canvas_w canvas_h
    canvas_min = min canvas_w canvas_h
    grid_min = desired_size
    grid_max = floor $ fromIntegral desired_size * canvas_max / canvas_min
    cell_size = floorf $ canvas_min / fromIntegral grid_min

roundf :: Double -> Double
roundf = fromIntegral @Int . round

floorf :: Double -> Double
floorf = fromIntegral @Int . floor

divf :: Double -> Int -> Double
divf x y = x / fromIntegral y

angularNeighborhood :: Double -> Double -> Double -> Double
angularNeighborhood center size a = exp (-((a - center) / size) ^ (2 :: Int))

lerp :: Double -> Double -> Double -> Double
lerp a x0 x1 = a * x1 + (1 - a) * x0

data Direction = R | D | L | U
  deriving (Show, Eq, Ord, Enum, Bounded)

data Rect
  = Rect
      { rx, ry, rw, rh :: !Double
      }
  deriving (Show, Eq)

unitRect :: Rect
unitRect = Rect (-1.0) (-1.0) 2.0 2.0

hsplit :: Rect -> [Rect]
hsplit = hsplitMod (/ 2)

hsplitGolden :: Rect -> [Rect]
hsplitGolden = hsplitMod (/ goldenRatio)

hsplitReverseGolden :: Rect -> [Rect]
hsplitReverseGolden = hsplitMod (* (1 - 1 / goldenRatio))

hsplitMod :: (Double -> Double) -> Rect -> [Rect]
hsplitMod modh r =
  [ r {rh = h'}
  , r {rh = rh r - h', ry = ry r + h'}
  ]
  where
    h' = modh $ rh r

vsplit :: Rect -> [Rect]
vsplit = vsplitMod (/ 2)

vsplitGolden :: Rect -> [Rect]
vsplitGolden = vsplitMod (/ goldenRatio)

vsplitReverseGolden :: Rect -> [Rect]
vsplitReverseGolden = vsplitMod (* (1 - 1 / goldenRatio))

vsplitMod :: (Double -> Double) -> Rect -> [Rect]
vsplitMod modw r =
  [ r {rw = w'}
  , r {rw = rw r - w', rx = rx r + w'}
  ]
  where
    w' = modw $ rw r

goldenRatio :: Double
goldenRatio = 1.618

opposite :: Direction -> Direction
opposite = \case
  R -> L
  D -> U
  L -> R
  U -> D

data CellSpec = Y | N

data GridSpec = GridSpec [[CellSpec]]

rectangularGridSpec :: Int -> Int -> GridSpec
rectangularGridSpec colCount rowCount =
  GridSpec (replicate rowCount (replicate colCount Y))

tesselate :: Int -> GridSpec -> GridSpec
tesselate n (GridSpec rows) = GridSpec (concatMap f rows)
  where
    f row = dup (concatMap dup row)
    dup x = replicate n x

calculateCellSize :: (Double, Double) -> GridSpec -> Double
calculateCellSize (w, h) (GridSpec gs) =
  let colCount = case gs of
        (row : _) -> length row
        _ -> 0
      rowCount = length gs
  in min (w / fromIntegral colCount) (h / fromIntegral rowCount)
