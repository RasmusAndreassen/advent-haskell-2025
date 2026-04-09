module Main (main) where

import Control.Arrow
import Data.Function (on)
import Data.Map (Map, (!))
import Data.Map qualified as Map
import GHC.Real (infinity)
import Point

getLines :: IO [String]
getLines = lines <$> getContents

nn d as =
  let mp = Map.fromList $ (id &&& const infinity) <$> as
      o = d `on` (mp !)
   in mp

main :: IO ()
main = do
  rows <- getLines

  let points :: [Point] = read <$> rows

  print $ length rows
