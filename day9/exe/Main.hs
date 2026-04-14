module Main (main) where

import Util
import Vector

diff :: (Num a) => a -> a -> a
diff = ((+ 1) . abs) .: (-)

area :: Vector -> Vector -> Int
area (V x1 y1) (V x2 y2) = x1 `diff` x2 * y1 `diff` y2

main :: IO ()
main = do
  points :: [Vector] <- map read . lines <$> getContents
