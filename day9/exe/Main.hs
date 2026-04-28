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

maxSquare :: (Vector, Vector) -> Matrix Tile -> Int -> Int
maxSquare (p, q) m max' =
  let a = area p q
      colored = allOf (sub (p ^. coords) (q ^. coords) . eachElem) (/= W) m
   in if a > max'
        && colored
        then a
        else max'

data Dir
  = Hor
  | Vert
  | Flat
  deriving (Show, Eq)

data Tile = W | R | G Dir deriving (Show, Eq)

data Location
  = Outside
  | Inside
  | TopEdge
  | BottomEdge
  | LeftEdge
  | RightEdge
  deriving (Show, Eq)

colorFloor :: Vector -> State (Maybe Vector, Matrix Tile) ()
colorFloor p1@(V x1 y1) = do
  p0@(V x0 y0) <- use $ _1 . to (fromMaybe p1)
  floor <- use _2
  let (minX, maxX) = extrema traverse [x0, x1]
      (minY, maxY) = extrema traverse [y0, y1]
      dir = if minX == maxX then Hor else Vert

  _1 .= Just p1
  _2
    .= ( floor
           & sub (minX, minY) (maxX, maxY) . eachElem .~ G dir
           & elemAt (x0, y0) .~ R
           & elemAt (x1, y1) .~ R
       )

validIndex :: (Int, Int) -> Matrix a -> Bool
validIndex (i, j) m =
  let (maxi, maxj) = m ^. size
   in i > 1
        && j > 0
        && i <= maxi
        && j <= maxj

fillArea :: Matrix Tile -> Matrix Tile
fillArea m =
  m
    & eachRow %@~ \i row ->
      let map = imapAccumLOf traversed $
            \j location tile ->
              let cornerFromInside = case (m ! below, m ! above) of
                    (W, _) -> TopEdge
                    (G Hor, _) -> TopEdge
                    (G Vert, _) -> BottomEdge
                    (_, G Vert) -> TopEdge
                  cornerFromOutside = case (m ! below, m ! above) of
                    (W, _) -> BottomEdge
                    (G Vert, _) -> TopEdge
                    (G Hor, _) -> BottomEdge
                    (_, G Vert) -> BottomEdge
                  above = (i - 1, j)
                  below = (i + 1, j)
                  topBorder = not $ validIndex above m
                  bottomBorder = not $ validIndex above m
                  location' = case (location, tile) of
                    (Inside, W) -> Inside
                    (Inside, G _) -> RightEdge
                    (Inside, R) -> cornerFromInside
                    (Outside, W) -> Outside
                    (Outside, G _) -> LeftEdge
                    (Outside, R) -> cornerFromOutside
                    (LeftEdge, W) -> Inside
                    (LeftEdge, G _) -> RightEdge
                    (LeftEdge, R) -> cornerFromInside
                    (RightEdge, W) -> Outside
                    (RightEdge, G _) -> LeftEdge
                    (RightEdge, R) -> cornerFromOutside
                    (TopEdge, G _) -> TopEdge
                    (TopEdge, R) -> if not topBorder && m ! above == G Vert then LeftEdge else RightEdge
                    (BottomEdge, G _) -> TopEdge
                    (BottomEdge, R) -> if not bottomBorder && m ! below == G Vert then LeftEdge else RightEdge
                  tile' = if location' == Inside then G Flat else tile
               in (location', tile')
       in snd . map Outside $ row

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
