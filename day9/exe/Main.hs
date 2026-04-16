module Main (main) where

import Control.Arrow
import Control.Lens
import Control.Monad (guard)
import Control.Monad.State (State, gets)
import Data.Bifoldable (biminimum)
import Data.Matrix
import Data.Matrix.Lens (sub)
import Data.Maybe (fromMaybe)
import Data.Semigroup (Max (Max, getMax), Min (Min, getMin))
import GHC.Generics
import Util
import Vector

diff :: (Num a) => a -> a -> a
diff = ((+ 1) . abs) .: (-)

area :: Vector -> Vector -> Int
area (V x1 y1) (V x2 y2) = x1 `diff` x2 * y1 `diff` y2

extremaOf :: Getting (Min a, Max a) s a -> s -> (a, a)
extremaOf f =
  let wrap = Const . (Min &&& Max)
      unwrap = (getMin *** getMax) . getConst
   in unwrap . f wrap

between :: (Vector, Vector) -> Vector -> Bool
between b (V vx vy) =
  let (minx, maxx) = extremaOf (traverse . x) b
      (miny, maxy) = extremaOf (traverse . y) b
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

data Tile = White | Red | Green deriving (Show, Eq, Enum)

colorFloor :: Vector -> Matrix Tile -> State (Maybe Vector) (Matrix Tile)
colorFloor p@(V x1 y1) floor = do
  (V x0 y0) <- gets $ fromMaybe p
  let (minX, maxX) = extremaOf traverse [x0, x1]
      (minY, maxY) = extremaOf traverse [y0, y1]
  return $ floor & sub (minX, minY) (maxX, maxY) . traverse .~ Green

main :: IO ()
main = do
  tiles :: [Vector] <- map read . lines <$> getContents
  let (baseX, _boundX) = extremaOf (traverse . x) tiles
      (baseY, _boundY) = extremaOf (traverse . y) tiles
      base = -V baseX baseY
      boundX = _boundX - baseX
      boundY = _boundY - baseY
      adjustedTiles = tiles <&> (+ base)
      floor = matrix boundX boundY (const White)

  print floor
