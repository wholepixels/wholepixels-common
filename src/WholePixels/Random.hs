module WholePixels.Random where

import Control.Monad.Random
import Relude
import qualified System.Random.Shuffle
import WholePixels.Color
import WholePixels.Geometry

disturbedSequence :: MonadRandom m => [Double] -> Double -> m [Double]
disturbedSequence xs amp = do
  dxs <- replicateM (length xs) $ getRandomR (-amp, amp)
  pure $ zipWith (+) xs dxs

filterRandomly :: MonadRandom m => Double -> [a] -> m [a]
filterRandomly probability xs = do
  ps <- replicateM (length xs) $ getRandomR (0, 1)
  pure [x | (x, p) <- zip xs ps, p < probability]

coinToss :: MonadRandom m => m Bool
coinToss = uniform [False, True]

genGrid :: MonadRandom m => Int -> Int -> m a -> m [(Int, Int, a)]
genGrid colCount rowCount genElement =
  genGridWithBoundaries colCount rowCount (const genElement)

genGridWithBoundaries :: MonadRandom m => Int -> Int -> ([Direction] -> m a) -> m [(Int, Int, a)]
genGridWithBoundaries colCount rowCount genElement = do
  let numberGrid = [(x, y) | y <- [0.. rowCount - 1], x <- [0.. colCount - 1]]
  forM numberGrid $ \(x, y) -> do
    let dirs = [R | x == 0] <> [D | y == 0] <> [L | x == colCount - 1] <> [U | y == rowCount - 1]
    (x,y,) <$> genElement dirs

genGrid' :: MonadRandom m => GridSpec -> ([Direction] -> m a) -> m [(Int, Int, a)]
genGrid' (GridSpec gridSpec) genElement = do
  let rowCount = length gridSpec
      colCount = case gridSpec of
        [] -> 0
        (row : _) -> length row
      numberedGrid :: [(Int, Int, CellSpec)]
      numberedGrid =
        concat $
          zipWith
            (\j row -> map (\(i, c) -> (i, j, c)) row)
            [0.. rowCount]
            ( map
              (zip [0.. colCount])
              gridSpec
            )
  fmap catMaybes . forM numberedGrid $ \(x, y, c) -> do
    let dirs = [R | x == 0] <> [D | y == 0] <> [L | x == colCount - 1] <> [U | y == rowCount - 1]
    case c of
      N -> pure Nothing
      Y -> (Just . (x,y,)) <$> genElement dirs

probably :: MonadRandom m => Double -> a -> m (Maybe a)
probably probability thing = do
  x <- getRandomR (0, 1)
  pure $
    if x < probability
    then Just thing
    else Nothing

shuffleM :: MonadRandom m => [a] -> m [a]
shuffleM = System.Random.Shuffle.shuffleM

data PaletteStrategy
  = Analogous
  | Complementary
  | SplitComplementary
  | Triangle

genPalette :: MonadRandom m => m Palette
genPalette = do
  strategy <- uniform [Analogous, Complementary, SplitComplementary, Triangle]
  genPaletteWithStrategy strategy

genPaletteWithStrategy :: MonadRandom m => PaletteStrategy -> m Palette
genPaletteWithStrategy strategy = do
  baseHue <- getRandomR (0, 360)
  baseSaturation <- getRandomR (0, 1)
  baseValue <- getRandomR (0, 0.5)
  bgToFgHueDifference <- uniform [180, 0, 30, 120, 240]
  let baseColor = HSV baseHue baseSaturation baseValue
      c = HSV (fixHue $ baseHue + bgToFgHueDifference) 1 1
      colors = case strategy of
        Analogous -> take 4 $ analogous c
        Complementary -> take 3 (analogous c) <> [complementary c]
        SplitComplementary ->
          let (c1, c2) = splitComplementary c
           in [c, c1, c2]
        Triangle ->
          let (c1, c2) = splitTriangle c
           in [c, c1, c2]
  pure $ Palette {..}

genMonochromePaletteForColor :: MonadRandom m => HSV -> m Palette
genMonochromePaletteForColor baseColor@(HSV bh bs bv) = do
  let colors = [HSV bh bs v | v <- [bv + 0.15, bv + 0.2.. 1]]
  pure $ Palette {..}

genMonochromePalette :: MonadRandom m => Double -> m Palette
genMonochromePalette maxSaturation = do
  hue <- getRandomR (0, 360)
  saturation <- getRandomR (0.0, maxSaturation)
  baseValue <- getRandomR (0, 0.5)
  let baseColor = HSV hue saturation baseValue
      colors = [HSV hue saturation v | v <- [baseValue + 0.15, baseValue + 0.2.. 1]]
  pure $ Palette {..}

genColor' :: MonadRandom m => Palette -> m HSV
genColor' pal = genColor pal 0.5 1.0

genColor :: MonadRandom m => Palette -> Double -> Double -> m HSV
genColor Palette {..} expected sigma2 = do
  let colorCount = length (baseColor : take 10 colors)
      colorPositions = take colorCount [0.0, (1.0 / fromIntegral colorCount)..]
      weights =
        map
          (\p -> toRational $ 1000.0 * exp (-(p - expected) * (p - expected) / sigma2))
          colorPositions
  weighted $ zip (baseColor : colors) weights

genRectSubdivision' :: forall m. MonadRandom m => Int -> Rect -> m [(Int, Rect)]
genRectSubdivision' max_depth r = foldr (>=>) pure (replicate max_depth step) [(0, r)]
  where
    step :: [(Int, Rect)] -> m [(Int, Rect)]
    step rs = concat <$> mapM go rs
    go :: (Int, Rect) -> m [(Int, Rect)]
    go (level, r') = do
      let hbonusWeight = if rh r' > rw r' then 15 else 0
          vbonusWeight = if rw r' > rh r' then 15 else 0
      f <-
        weighted
          [ (pure, 10)
          , (hsplit, 1 + hbonusWeight)
          , (hsplitGolden, 3 + hbonusWeight)
          , (hsplitReverseGolden, 3 + hbonusWeight)
          , (vsplit, 1 + vbonusWeight)
          , (vsplitGolden, 3 + vbonusWeight)
          , (vsplitReverseGolden, 3 + vbonusWeight)
          ]
      let rs' = f r'
          dlevel = case rs' of
            [_] -> 0
            _ -> 1
      pure $ map (level + dlevel,) rs'

genRectSubdivision :: forall m. MonadRandom m => Int -> Rect -> m [Rect]
genRectSubdivision depth r = map snd <$> genRectSubdivision' depth r
