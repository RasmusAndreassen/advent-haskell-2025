module Main (main) where

import Control.Arrow
import Control.Lens
import Control.Monad (guard, when)
import Control.Monad.State (MonadState (get, put), State, evalState, gets)
import Data.Bifoldable (biminimum)
import Data.Matrix
import Data.Matrix.Lens
import Data.Maybe (fromMaybe)
import Data.Semigroup (Max (Max, getMax), Min (Min, getMin))
import Util
import Vector

diff :: (Num a) => a -> a -> a
diff = ((+ 1) . abs) .: (-)

area :: Vector -> Vector -> Int
area (V x1 y1) (V x2 y2) = x1 `diff` x2 * y1 `diff` y2

extrema :: Getting (Min a, Max a) s a -> s -> (a, a)
extrema f =
  let wrap = Const . (Min &&& Max)
      unwrap = (getMin *** getMax) . getConst
   in unwrap . f wrap

between :: (Vector, Vector) -> Vector -> Bool
between b (V vx vy) =
  let (minx, maxx) = extrema (traverse . x) b
      (miny, maxy) = extrema (traverse . y) b
   in and
        [ minx < vx,
          vx < maxx,
          miny < vy,
          vy < maxy
        ]

squares :: [Vector] -> [Int]
squares vs = do
  v1 <- vs
  v2 <- takeWhile (/= v1) vs
  guard $ none (between (v1, v2)) vs
  return $ area v1 v2

data Tile = W | R | G deriving (Show, Eq, Enum)

colorFloor :: Vector -> State (Maybe Vector, Matrix Tile) ()
colorFloor p1@(V x1 y1) = do
  p0@(V x0 y0) <- use $ _1 . to (fromMaybe p1)
  floor <- use _2
  let (minX, maxX) = extrema traverse [x0, x1]
      (minY, maxY) = extrema traverse [y0, y1]

  _1 .= Just p1
  _2
    .= ( floor
           & sub (minX, minY) (maxX, maxY) . eachElem .~ G
           & elemAt (x0, y0) .~ R
           & elemAt (x1, y1) .~ R
       )

brush :: Tile -> State Tile Tile
brush t = do
  b <- get
  case b of
    W -> do
      put t
      return t
    G -> do
      when (t == G) $
        put W
      when (t == R) $
        put R
      return $
        if t == R then R else G
    R -> do
      when (t /= G) $
        put W
      return t

fillArea :: Matrix Tile -> Matrix Tile
fillArea =
  eachRow %~ \row ->
    let row' = mapM brush row
        r = evalState row' W
     in r

main :: IO ()
main = do
  tiles :: [Vector] <- map read . lines <$> getContents
  let (baseX, _boundX) = extrema (traverse . x) tiles
      (baseY, _boundY) = extrema (traverse . y) tiles
      adjustment = V baseX baseY
      boundX = _boundX - baseX + 1
      boundY = _boundY - baseY + 1
      adjustedTiles = tiles |> head tiles <&> (+ (-adjustment + 1))
      floor = matrix boundX boundY (const W)
      coloring = mapM_ colorFloor adjustedTiles
      floor' = ((Nothing, floor) &~ coloring) ^. _2
      floor'' = fillArea floor'

  print floor''
