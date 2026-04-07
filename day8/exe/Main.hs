module Main (main) where

import Arith
import Control.Arrow
import Data.Foldable (minimumBy)
import Data.List (sortBy)
import Data.Ord (comparing)
import Point
import Prelude hiding (negate, (+), (-))

getLines :: IO [String]
getLines = lines <$> getContents

split :: a -> (a, a)
split a = (a, a)

asTriplets :: Point -> Point -> Ordering
asTriplets p q =
  let byX = comparing _x p q
      byY = comparing _y p q
      byZ = comparing _z p q
   in byX <> byY <> byZ

main :: IO ()
main = do
  rows <- getLines
  let points = map read rows :: [Point]
      ordered = sortBy asTriplets points

  print $ length rows
