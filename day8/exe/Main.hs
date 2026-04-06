module Main (main) where

import Control.Arrow
import Data.Foldable (minimumBy)
import Data.Ord (comparing)
import Point

getLines :: IO [String]
getLines = lines <$> getContents

main :: IO ()
main = do
  rows <- getLines
  let (f : points) :: [Point] = map read rows
      moyori = minimumBy (comparing $ distance f) points

  print $ length rows
