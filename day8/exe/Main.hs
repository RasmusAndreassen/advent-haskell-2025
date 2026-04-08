module Main (main) where

import Point

getLines :: IO [String]
getLines = lines <$> getContents

main :: IO ()
main = do
  rows <- getLines

  let points :: [Point] = read <$> rows

  print $ length rows
